/*

 BadaSystem
 Program       : HbORM - Harbour Object-Relational Mapping
 Module        : Class for building complex queries
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrin
 Email         : marvijarrin@gmail.com
 Date          : 13/07/2025
 Update        : 05/08/2025
 Rev           : 0.1
 Description: Class for building complex queries

*/

#include "hbclass.ch"
#include "minigui.ch"

/**
 * Query builder class for HbORM
 * class HbORMQuery
 * description Provides fluent interface for building complex database queries
 */
CLASS HbORMQuery
   EXPORTED:
      VAR oORM         AS OBJECT        /** Target HbORM object */
      VAR aConditions  AS ARRAY         /** Array of WHERE conditions */
      VAR aFields      AS ARRAY         /** Fields to select */
      VAR cOrder       AS STRING        /** Order by field */
      VAR lDesc        AS LOGICAL       /** Descending order flag */
      VAR nLimit       AS NUMERIC       /** Record limit */
      VAR aJoins       AS ARRAY         /** Join definitions */
      VAR cError       AS STRING        /** Last error message */


      METHOD New(oORM) CONSTRUCTOR              /* Class constructor */
      METHOD Select(aFields)                    /* Specifies fields to select */
      METHOD Where(cField, cOperator, xValue)   /* Adds a WHERE condition */
      METHOD OrWhere(cField, cOperator, xValue) /* Adds an OR WHERE condition */
      METHOD WhereIn(cField, aValues)           /* Adds a WHERE IN condition */
      METHOD WhereNotIn(cField, aValues)        /* Adds a WHERE NOT IN condition */
      METHOD WhereBetween(cField, xValue1, xValue2)  /* Adds a WHERE BETWEEN condition */
      METHOD WhereNull(cField)                       /* Adds a WHERE IS NULL condition */
      METHOD WhereNotNull(cField)                    /* Adds a WHERE IS NOT NULL condition */
      METHOD OrderBy(cField, lDesc)                  /* Specifies result ordering */
      METHOD Limit(nLimit)                           /* Specifies record limit */
      METHOD Join(cTable, cAlias, cCondition, cType) /* Adds a table join */
      METHOD Get()      /* Executes query and returns results */
      METHOD First()    /* Gets first matching record */
      METHOD Count()    /* Counts matching records */
      METHOD ToSQL()    /* Generates SQL equivalent (for debugging) */
      METHOD GetError() /* Gets last error message */

   PROTECTED:
      METHOD _BuildCondition()                 /* Builds condition evaluation block */
      METHOD _EvalCondition(hRow, aCondition)  /* Evaluates a single condition */
      METHOD _FieldValue(hRow, cField)         /* Gets field value from record */
ENDCLASS

/**
 * Constructor method
 * method New
 * param {HbORM} oORM - Target ORM object
 * returns {HbORMQuery} New query builder instance
 */
METHOD New(oORM) CLASS HbORMQuery
   ::oORM        := oORM
   ::aConditions := {}
   ::aFields     := {}
   ::aJoins      := {}
   ::cOrder      := ""
   ::lDesc       := .F.
   ::nLimit      := 0
   ::cError      := ""

   RETURN Self

/**
 * Specifies fields to select
 * method Select
 * param {Array|String} aFields - Fields to select (array or single field)
 * returns {HbORMQuery} Self for method chaining
 */
METHOD Select(aFields) CLASS HbORMQuery
   IF ValType(aFields) == "A"
      ::aFields := aFields
   ELSEIF ValType(aFields) == "C"
      ::aFields := {aFields}
   ENDIF

   RETURN Self

/**
 * Adds a WHERE condition
 * method Where
 * param {String} cField - Field name
 * param {String} cOperator - Comparison operator
 * param {*} xValue - Comparison value
 * returns {HbORMQuery} Self for method chaining
*/
METHOD Where(cField, cOperator, xValue) CLASS HbORMQuery

   AAdd(::aConditions, {"AND", cField, Upper(cOperator), xValue})

   RETURN Self

/**
 * Adds an OR WHERE condition
 * method OrWhere
 * param {String} cField - Field name
 * param {String} cOperator - Comparison operator
 * param {*} xValue - Comparison value
 * returns {HbORMQuery} Self for method chaining
 */
METHOD OrWhere(cField, cOperator, xValue) CLASS HbORMQuery

   AAdd(::aConditions, {"OR", cField, Upper(cOperator), xValue})

   RETURN Self

/**
 * Adds a WHERE IN condition
 * method WhereIn
 * param {String} cField - Field name
 * param {Array} aValues - Array of values
 * returns {HbORMQuery} Self for method chaining
*/
METHOD WhereIn(cField, aValues) CLASS HbORMQuery
   AAdd(::aConditions, {"AND", cField, "IN", aValues})

   RETURN Self


/**
 * Adds a WHERE NOT IN condition
 * method WhereNotIn
 * param {String} cField - Field name
 * param {Array} aValues - Array of values
 * returns {HbORMQuery} Self for method chaining
*/
METHOD WhereNotIn(cField, aValues) CLASS HbORMQuery
   AAdd(::aConditions, {"AND", cField, "NOT IN", aValues})

   RETURN Self

/**
 * Adds a WHERE BETWEEN condition
 * method WhereBetween
 * param {String} cField - Field name
 * param {*} xValue1 - Lower bound value
 * param {*} xValue2 - Upper bound value
 * returns {HbORMQuery} Self for method chaining
*/
METHOD WhereBetween(cField, xValue1, xValue2) CLASS HbORMQuery
   AAdd(::aConditions, {"AND", cField, "BETWEEN", {xValue1, xValue2}})

   RETURN Self

/**
 * Adds a WHERE IS NULL condition
 * method WhereNull
 * param {String} cField - Field name
 * returns {HbORMQuery} Self for method chaining
*/
METHOD WhereNull(cField) CLASS HbORMQuery
   AAdd(::aConditions, {"AND", cField, "IS NULL", NIL})

   RETURN Self

/**
 * Adds a WHERE IS NOT NULL condition
 * method WhereNotNull
 * param {String} cField - Field name
 * returns {HbORMQuery} Self for method chaining
 */
METHOD WhereNotNull(cField) CLASS HbORMQuery
   AAdd(::aConditions, {"AND", cField, "IS NOT NULL", NIL})

   RETURN Self

/**
 * Specifies result ordering
 * method OrderBy
 * param {String} cField - Field to order by
 * param {Boolean} [lDesc=.F.] - Descending order flag
 * returns {HbORMQuery} Self for method chaining
*/
METHOD OrderBy(cField, lDesc) CLASS HbORMQuery
   ::cOrder := cField
   ::lDesc := IIF(lDesc == NIL, .F., lDesc)

   RETURN Self

/**
 * Specifies record limit
 * method Limit
 * param {Numeric} nLimit - Maximum number of records
 * returns {HbORMQuery} Self for method chaining
 */
METHOD Limit(nLimit) CLASS HbORMQuery
   ::nLimit := nLimit

   RETURN Self

/**
 * Adds a table join
 * method Join
 * param {String} cTable - Table to join
 * param {String} cAlias - Table alias
 * param {String} cCondition - Join condition
 * param {String} [cType="INNER"] - Join type
 * returns {HbORMQuery} Self for method chaining
 */
METHOD Join(cTable, cAlias, cCondition, cType) CLASS HbORMQuery
   DEFAULT cType := "INNER"

   AAdd(::aJoins, {Upper(cType), cTable, cAlias, cCondition})

   RETURN Self


/**
 * Executes query and returns results
 * method Get
 * returns {Array} Array of result records
 */
METHOD Get() CLASS HbORMQuery
   LOCAL aResult := {}
   LOCAL hRow
   LOCAL nCount := 0
   LOCAL bCondition := ::_BuildCondition()

   IF !::oORM:lOpen
      IF !::oORM:Open()
         ::cError := ::oORM:GetError()
         RETURN aResult
      ENDIF
   ENDIF

   // Apply ordering if defined
   IF !Empty(::cOrder)
      ::oORM:SetOrder(::cOrder)
   ENDIF

   ::oORM:GoTop()

   DO WHILE !::oORM:Eof() .AND. (::nLimit == 0 .OR. nCount < ::nLimit)
      hRow := ::oORM:GetRow()

      IF Eval(bCondition, hRow)
         IF !Empty(::aFields)
            hRow := HbORM_FilterFields(hRow, ::aFields)
         ENDIF

         AAdd(aResult, hRow)
         nCount++
      ENDIF

      ::oORM:Skip()
   ENDDO

   RETURN aResult


/**
 * Gets first matching record
 * method First
 * returns {Hash} First matching record or NIL
*/
METHOD First() CLASS HbORMQuery
   LOCAL aResult := ::Limit(1):Get()

   RETURN IIF(Empty(aResult), NIL, aResult[1])


/**
 * Counts matching records
 * method Count
 * returns {Numeric} Number of matching records
*/
METHOD Count() CLASS HbORMQuery
   LOCAL nCount := 0
   LOCAL bCondition := ::_BuildCondition()

   IF !::oORM:lOpen
      IF !::oORM:Open()
         ::cError := ::oORM:GetError()
         RETURN 0
      ENDIF
   ENDIF

   ::oORM:GoTop()

   DO WHILE !::oORM:Eof()
      IF Eval(bCondition, ::oORM:GetRow())
         nCount++
      ENDIF

      ::oORM:Skip()
   ENDDO

   RETURN nCount

/**
 * Generates SQL equivalent (for debugging)
 * method ToSQL
 * returns {String} Generated SQL query
 */
METHOD ToSQL() CLASS HbORMQuery
   LOCAL cSQL := "SELECT "
   LOCAL cWhere := ""
   LOCAL cJoin := ""
   LOCAL aCondition
   LOCAL aJoin

   // Fields
   IF Empty(::aFields)
      cSQL += "*"
   ELSE
      cSQL += ArrayToList(::aFields, ", ")
   ENDIF

   // Main table
   cSQL += " FROM " + ::oORM:cTable

   // Joins
   FOR EACH aJoin IN ::aJoins
      cSQL += " " + aJoin[1] + " JOIN " + aJoin[2]
      IF !Empty(aJoin[3])
         cSQL += " AS " + aJoin[3]
      ENDIF
      cSQL += " ON " + aJoin[4]
   NEXT

   // WHERE conditions
   IF !Empty(::aConditions)
      cSQL += " WHERE "

      FOR EACH aCondition IN ::aConditions
         IF !Empty(cWhere) .AND. aCondition[1] == "AND"
            cWhere += " AND "
         ELSEIF !Empty(cWhere) .AND. aCondition[1] == "OR"
            cWhere += " OR "
         ENDIF

         DO CASE
            CASE aCondition[3] == "="
               cWhere += aCondition[2] + " = " + ValToSQL(aCondition[4])

            CASE aCondition[3] == "!=" .OR. aCondition[3] == "<>"
               cWhere += aCondition[2] + " != " + ValToSQL(aCondition[4])

            CASE aCondition[3] == ">"
               cWhere += aCondition[2] + " > " + ValToSQL(aCondition[4])

            CASE aCondition[3] == ">="
               cWhere += aCondition[2] + " >= " + ValToSQL(aCondition[4])

            CASE aCondition[3] == "<"
               cWhere += aCondition[2] + " < " + ValToSQL(aCondition[4])

            CASE aCondition[3] == "<="
               cWhere += aCondition[2] + " <= " + ValToSQL(aCondition[4])

            CASE aCondition[3] == "IN"
               cWhere += aCondition[2] + " IN (" + ArrayToSQL(aCondition[4]) + ")"

            CASE aCondition[3] == "NOT IN"
               cWhere += aCondition[2] + " NOT IN (" + ArrayToSQL(aCondition[4]) + ")"

            CASE aCondition[3] == "BETWEEN"
               cWhere += aCondition[2] + " BETWEEN " + ValToSQL(aCondition[4][1]) + ;
                         " AND " + ValToSQL(aCondition[4][2])

            CASE aCondition[3] == "IS NULL"
               cWhere += aCondition[2] + " IS NULL"

            CASE aCondition[3] == "IS NOT NULL"
               cWhere += aCondition[2] + " IS NOT NULL"
         ENDCASE
      NEXT

      cSQL += cWhere
   ENDIF

   // Ordering
   IF !Empty(::cOrder)
      cSQL += " ORDER BY " + ::cOrder
      IF ::lDesc
         cSQL += " DESC"
      ENDIF
   ENDIF

   // Limit
   IF ::nLimit > 0
      cSQL += " LIMIT " + AllTrim(Str(::nLimit))
   ENDIF

   RETURN cSQL

/**
 * Gets last error message
 * method GetError
 * returns {String} Last error message
*/
METHOD GetError() CLASS HbORMQuery
   RETURN ::cError

/**
 * Builds condition evaluation block
 * method _BuildCondition
 * private
 * returns {Block} Condition evaluation block
*/
METHOD _BuildCondition() CLASS HbORMQuery
   LOCAL bCondition := {|hRow| .T. }
   LOCAL aCondition

   IF Empty(::aConditions)
      RETURN bCondition
   ENDIF

   bCondition := {|hRow|
      LOCAL lResult := .T.
      LOCAL lCurrent
      LOCAL nI

      FOR nI := 1 TO Len(::aConditions)
         aCondition := ::aConditions[nI]
         lCurrent := ::_EvalCondition(hRow, aCondition)

         IF nI == 1
            lResult := lCurrent
         ELSE
            IF aCondition[1] == "AND"
               lResult := lResult .AND. lCurrent
            ELSE // OR
               lResult := lResult .OR. lCurrent
            ENDIF
         ENDIF
      NEXT

      RETURN lResult
   }

   RETURN bCondition

/**
 * Evaluates a single condition
 * method _EvalCondition
 * private
 * param {Hash} hRow - Record data
 * param {Array} aCondition - Condition definition
 * returns {Boolean} Evaluation result
*/
METHOD _EvalCondition(hRow, aCondition) CLASS HbORMQuery
   LOCAL xValue := ::_FieldValue(hRow, aCondition[2])
   LOCAL xCompare := aCondition[4]

   DO CASE
      CASE aCondition[3] == "="
         RETURN xValue == xCompare

      CASE aCondition[3] == "!=" .OR. aCondition[3] == "<>"
         RETURN xValue != xCompare

      CASE aCondition[3] == ">"
         RETURN xValue > xCompare

      CASE aCondition[3] == ">="
         RETURN xValue >= xCompare

      CASE aCondition[3] == "<"
         RETURN xValue < xCompare

      CASE aCondition[3] == "<="
         RETURN xValue <= xCompare

      CASE aCondition[3] == "IN"
         RETURN AScan(xCompare, xValue) > 0

      CASE aCondition[3] == "NOT IN"
         RETURN AScan(xCompare, xValue) == 0

      CASE aCondition[3] == "BETWEEN"
         RETURN xValue >= xCompare[1] .AND. xValue <= xCompare[2]

      CASE aCondition[3] == "IS NULL"
         RETURN xValue == NIL

      CASE aCondition[3] == "IS NOT NULL"
         RETURN xValue != NIL
   ENDCASE

   RETURN .F.

/**
 * Gets field value from record
 * method _FieldValue
 * private
 * param {Hash} hRow - Record data
 * param {String} cField - Field name
 * returns {*} Field value
*/
METHOD _FieldValue(hRow, cField) CLASS HbORMQuery
   LOCAL nPos := At(".", cField)

   IF nPos > 0
      // Related table field (table.field)
      // Would require additional implementation for joins
      RETURN NIL
   ENDIF

   RETURN hRow[cField]

/**
 * Converts value to SQL format
 * static
 * param {*} xValue - Value to convert
 * returns {String} SQL-formatted value
 */
STATIC FUNCTION ValToSQL(xValue)
   DO CASE
      CASE xValue == NIL
         RETURN "NULL"
      CASE ValType(xValue) == "C"
         RETURN "'" + StrTran(xValue, "'", "''") + "'"
      CASE ValType(xValue) == "N"
         RETURN AllTrim(Str(xValue))
      CASE ValType(xValue) == "D"
         RETURN "'" + DToC(xValue) + "'"
      CASE ValType(xValue) == "L"
         RETURN IIF(xValue, "1", "0")
      OTHERWISE
         RETURN "'" + STR(xValue) + "'"
   ENDCASE

   RETURN ""

/**
 * Converts array to SQL format
 * static
 * param {Array} aValues - Array of values
 * returns {String} SQL-formatted list
 */
STATIC FUNCTION ArrayToSQL(aValues)
   LOCAL cResult := ""
   LOCAL xValue

   FOR EACH xValue IN aValues
      IF !Empty(cResult)
         cResult += ", "
      ENDIF
      cResult += ValToSQL(xValue)
   NEXT

   RETURN cResult

/**
 * Filters hash to include only specified fields
 * static
 * param {Hash} hRow - Record data
 * param {Array} aFields - Fields to include
 * returns {Hash} Filtered record data
 */
STATIC FUNCTION HbORM_FilterFields(hRow, aFields)
   LOCAL hResult := {=>}
   LOCAL cField

   FOR EACH cField IN aFields
      IF hb_HHasKey(hRow, cField)
         hResult[cField] := hRow[cField]
      ENDIF
   NEXT

   RETURN hResult

/**
 * Sorts array of hashes by specified field
 * static
 * param {Array} aData - Data to sort
 * param {String} cField - Field to sort by
 * param {Boolean} [lDesc=.F.] - Descending order flag
 * returns {Array} Sorted array
 */
STATIC FUNCTION HbORM_SortArray(aData, cField, lDesc)
   LOCAL bCompare

   DEFAULT lDesc := .F.

   bCompare := {|x, y|
      LOCAL nResult

      DO CASE
         CASE x[cField] < y[cField]
            nResult := -1
         CASE x[cField] > y[cField]
            nResult := 1
         OTHERWISE
            nResult := 0
      ENDCASE

      RETURN IIF(lDesc, -nResult, nResult)
   }

   ASort(aData,,, bCompare)

   RETURN aData
