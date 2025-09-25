/*

 BadaSystem
 Program       : HbORM - Harbour Object-Relational Mapping
 Module        : HbORM Utils - Utilities for Harbour's HbORM
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrin
 Email         : marvijarrin@gmail.com
 Date          : 18/07/2025
 Update        : 05/08/2025
 Rev           : 0.1
 Description: Utility classes to extend HbORM

*/

#include "hbclass.ch"
#include "minigui.ch"

/**
 * Data validation class for HbORM
 * class HbORMValidator
 * description Provides comprehensive data validation capabilities for ORM models
 */
CREATE CLASS HbORMValidator
   EXPORTED:
      /* Array containing validation rules
       * var {Array} aRules
       */
      VAR aRules  AS ARRAY

      /**
       * Array containing validation errors
       * var {Array} aErrors
       */
      VAR aErrors  AS ARRAY

      /**
       * Constructor method
       * method New
       * returns {HbORMValidator} New instance of validator
       */
      METHOD New() CONSTRUCTOR
      METHOD AddRule(cField, cType, xValue, cMessage)  /* Adds a validation rule to the validator */
      METHOD Validate(hData)   /* Validates data against all defined rules */
      METHOD GetErrors()       /* Returns all validation errors */
      METHOD HasErrors()       /* Checks if any validation errors exist */
      METHOD ClearErrors()     /* Clears all validation errors */

   PROTECTED:
      METHOD _ValidateRequired(xValue, cField)    /* Validates that a field is required (not empty/NIL) */
      METHOD _ValidateType(xValue, cType, cField) /* Validates that a field matches the specified type */
      METHOD _ValidateMin(xValue, nMin, cField)   /* Validates minimum value constraints */
      METHOD _ValidateMax(xValue, nMax, cField)   /* Validates maximum value constraints */
      METHOD _ValidateLength(xValue, nLength, cField) /* Validates exact length constraints */
      METHOD _ValidateRegex(xValue, cRegex, cField) /* Validates against regular expression patterns */
      METHOD _ValidateEmail(xValue, cField)       /* Validates email format */
      METHOD _ValidateDate(xValue, cField)        /* Validates date fields */
      METHOD _ValidateCustom(xValue, bValidator, cField) /* Validates using custom validation functions */
      METHOD _AddError(cField, cMessage)          /* Adds a validation error to the errors collection */
ENDCLASS

/**
 * Class constructor
 */
METHOD New() CLASS HbORMValidator
   ::aRules := {}
   ::aErrors := {}

   RETURN Self


/**
 * Adds a validation rule
 * method AddRule
 * param {String} cField - Field name to validate
 * param {String} cType - Validation type (required|type|min|max|length|regex|email|date|custom)
 * param {*} xValue - Validation parameter (depends on type)
 * param {String} cMessage - Error message if validation fails
 * returns {HbORMValidator} Self for method chaining
*/
METHOD AddRule(cField, cType, xValue, cMessage) CLASS HbORMValidator
   AAdd(::aRules, {cField, cType, xValue, cMessage})

   RETURN Self

/**
 * Validates data against defined rules
 * method Validate
 * param {Hash} hData - Data to validate (field => value pairs)
 * returns {Boolean} .T. if validation passes, .F. otherwise
*/
METHOD Validate(hData) CLASS HbORMValidator
   LOCAL aRule
   LOCAL lValid := .T.

   ::ClearErrors()

   FOR EACH aRule IN ::aRules
      DO CASE
         CASE aRule[2] == "required"
            IF !::_ValidateRequired(hData[aRule[1]], aRule[1])
               ::_AddError(aRule[1], aRule[4])
               lValid := .F.
            ENDIF

         CASE aRule[2] == "type"
            IF !::_ValidateType(hData[aRule[1]], aRule[3], aRule[1])
               ::_AddError(aRule[1], aRule[4])
               lValid := .F.
            ENDIF

         CASE aRule[2] == "min"
            IF !::_ValidateMin(hData[aRule[1]], aRule[3], aRule[1])
               ::_AddError(aRule[1], aRule[4])
               lValid := .F.
            ENDIF

         CASE aRule[2] == "max"
            IF !::_ValidateMax(hData[aRule[1]], aRule[3], aRule[1])
               ::_AddError(aRule[1], aRule[4])
               lValid := .F.
            ENDIF

         CASE aRule[2] == "length"
            IF !::_ValidateLength(hData[aRule[1]], aRule[3], aRule[1])
               ::_AddError(aRule[1], aRule[4])
               lValid := .F.
            ENDIF

         CASE aRule[2] == "regex"
            IF !::_ValidateRegex(hData[aRule[1]], aRule[3], aRule[1])
               ::_AddError(aRule[1], aRule[4])
               lValid := .F.
            ENDIF

         CASE aRule[2] == "email"
            IF !::_ValidateEmail(hData[aRule[1]], aRule[1])
               ::_AddError(aRule[1], aRule[4])
               lValid := .F.
            ENDIF

         CASE aRule[2] == "date"
            IF !::_ValidateDate(hData[aRule[1]], aRule[1])
               ::_AddError(aRule[1], aRule[4])
               lValid := .F.
            ENDIF

         CASE aRule[2] == "custom"
            IF !::_ValidateCustom(hData[aRule[1]], aRule[3], aRule[1])
               ::_AddError(aRule[1], aRule[4])
               lValid := .F.
            ENDIF
      ENDCASE
   NEXT

   RETURN lValid

/**
 * Gets all validation errors
 * method GetErrors
 * returns {Array} Array of errors in format {{field, message}, ...}
 */
METHOD GetErrors() CLASS HbORMValidator
   RETURN ::aErrors


/**
 * Checks if there are validation errors
 * method HasErrors
 * returns {Boolean} .T. if errors exist, .F. otherwise
 */
METHOD HasErrors() CLASS HbORMValidator
   RETURN Len(::aErrors) > 0

/**
 * Clears all validation errors
 * method ClearErrors
 * returns {HbORMValidator} Self for method chaining
*/
METHOD ClearErrors() CLASS HbORMValidator
   ::aErrors := {}

   RETURN Self

/**
 * Validates required fields
 * method _ValidateRequired
 * private
 * param {*} xValue - Value to validate
 * param {String} cField - Field name being validated
 * returns {Boolean} Validation result
*/
METHOD _ValidateRequired(xValue, cField) CLASS HbORMValidator
   IF xValue == NIL
      RETURN .F.
   ENDIF

   DO CASE
      CASE ValType(xValue) == "C"
         RETURN !Empty(xValue)
      CASE ValType(xValue) == "N"
         RETURN .T.
      CASE ValType(xValue) == "D"
         RETURN !Empty(xValue)
      CASE ValType(xValue) == "L"
         RETURN .T.
      CASE ValType(xValue) == "A"
         RETURN Len(xValue) > 0
      CASE ValType(xValue) == "H"
         RETURN Len(hb_HKeys(xValue)) > 0
      OTHERWISE
         RETURN .F.
   ENDCASE

   RETURN .T.

/**
 * Validates field types
 * method _ValidateType
 * private
 * param {*} xValue - Value to validate
 * param {String} cType - Expected type
 * param {String} cField - Field name being validated
 * returns {Boolean} Validation result
 */
METHOD _ValidateType(xValue, cType, cField) CLASS HbORMValidator
   IF xValue == NIL
      RETURN .T.  // NIL fields are not type-validated
   ENDIF

   DO CASE
      CASE cType == "C"
         RETURN ValType(xValue) == "C"
      CASE cType == "N"
         RETURN ValType(xValue) == "N"
      CASE cType == "D"
         RETURN ValType(xValue) == "D"
      CASE cType == "L"
         RETURN ValType(xValue) == "L"
      CASE cType == "A"
         RETURN ValType(xValue) == "A"
      CASE cType == "H"
         RETURN ValType(xValue) == "H"
      CASE cType == "B"
         RETURN ValType(xValue) == "B"
      OTHERWISE
         RETURN .F.
   ENDCASE

   RETURN .T.


/**
 * Validates minimum values
 * method _ValidateMin
 * private
 * param {*} xValue - Value to validate
 * param {Numeric} nMin - Minimum allowed value
 * param {String} cField - Field name being validated
 * returns {Boolean} Validation result
*/
METHOD _ValidateMin(xValue, nMin, cField) CLASS HbORMValidator
   IF xValue == NIL
      RETURN .T.  // NIL fields are not min-validated
   ENDIF

   DO CASE
      CASE ValType(xValue) == "N"
         RETURN xValue >= nMin
      CASE ValType(xValue) == "C"
         RETURN Len(xValue) >= nMin
      CASE ValType(xValue) == "A"
         RETURN Len(xValue) >= nMin
      CASE ValType(xValue) == "H"
         RETURN Len(hb_HKeys(xValue)) >= nMin
      OTHERWISE
         RETURN .F.
   ENDCASE

   RETURN .T.

/**
 * Validates maximum values
 * method _ValidateMax
 * private
 * param {*} xValue - Value to validate
 * param {Numeric} nMax - Maximum allowed value
 * param {String} cField - Field name being validated
 * returns {Boolean} Validation result
*/
METHOD _ValidateMax(xValue, nMax, cField) CLASS HbORMValidator
   IF xValue == NIL
      RETURN .T.  // NIL fields are not max-validated
   ENDIF

   DO CASE
      CASE ValType(xValue) == "N"
         RETURN xValue <= nMax
      CASE ValType(xValue) == "C"
         RETURN Len(xValue) <= nMax
      CASE ValType(xValue) == "A"
         RETURN Len(xValue) <= nMax
      CASE ValType(xValue) == "H"
         RETURN Len(hb_HKeys(xValue)) <= nMax
      OTHERWISE
         RETURN .F.
   ENDCASE

   RETURN .T.

/**
 * Validates exact lengths
 * method _ValidateLength
 * private
 * param {*} xValue - Value to validate
 * param {Numeric} nLength - Expected length
 * param {String} cField - Field name being validated
 * returns {Boolean} Validation result
*/
METHOD _ValidateLength(xValue, nLength, cField) CLASS HbORMValidator
   IF xValue == NIL
      RETURN .T.  // NIL fields are not length-validated
   ENDIF

   DO CASE
      CASE ValType(xValue) == "C"
         RETURN Len(xValue) == nLength
      CASE ValType(xValue) == "A"
         RETURN Len(xValue) == nLength
      CASE ValType(xValue) == "H"
         RETURN Len(hb_HKeys(xValue)) == nLength
      OTHERWISE
         RETURN .F.
   ENDCASE

   RETURN .T.

/**
 * Validates against regular expressions
 * method _ValidateRegex
 * private
 * param {*} xValue - Value to validate
 * param {String} cRegex - Regular expression pattern
 * param {String} cField - Field name being validated
 * returns {Boolean} Validation result
*/
METHOD _ValidateRegex(xValue, cRegex, cField) CLASS HbORMValidator
   IF xValue == NIL
      RETURN .T.  // NIL fields are not regex-validated
   ENDIF

   IF ValType(xValue) != "C"
      RETURN .F.
   ENDIF

   // Basic regex validation implementation
   // In Harbour, hb_RegExMatch() could be used if available
   // This is a simplified implementation

   DO CASE
      CASE cRegex == "^[0-9]+$"  // Numbers only
         RETURN xValue == AllTrim(Str(Val(xValue)))
      CASE cRegex == "^[A-Za-z]+$"  // Letters only
         // TODO: Implement proper validation
      CASE cRegex == "^[A-Za-z0-9]+$"  // Alphanumeric
         // TODO: Implement proper validation
      OTHERWISE
         RETURN .T.  // If pattern not recognized, consider valid
   ENDCASE

   RETURN .T.

/**
 * Validates email format
 * method _ValidateEmail
 * private
 * param {*} xValue - Value to validate
 * param {String} cField - Field name being validated
 * returns {Boolean} Validation result
*/
METHOD _ValidateEmail(xValue, cField) CLASS HbORMValidator
   IF xValue == NIL
      RETURN .T.  // NIL fields are not email-validated
   ENDIF

   IF ValType(xValue) != "C"
      RETURN .F.
   ENDIF

   // Basic email validation
   RETURN "" $ xValue .AND. "." $ xValue .AND. !" " $ xValue

/**
 * Validates date fields
 * method _ValidateDate
 * private
 * param {*} xValue - Value to validate
 * param {String} cField - Field name being validated
 * returns {Boolean} Validation result
*/
METHOD _ValidateDate(xValue, cField) CLASS HbORMValidator
   IF xValue == NIL
      RETURN .T.  // NIL fields are not date-validated
   ENDIF

   RETURN ValType(xValue) == "D"

/**
 * Validates using custom functions
 * method _ValidateCustom
 * private
 * param {*} xValue - Value to validate
 * param {Block} bValidator - Custom validation function
 * param {String} cField - Field name being validated
 * returns {Boolean} Validation result
*/
METHOD _ValidateCustom(xValue, bValidator, cField) CLASS HbORMValidator
   IF xValue == NIL
      RETURN .T.  // NIL fields are not custom-validated
   ENDIF

   IF ValType(bValidator) != "B"
      RETURN .F.
   ENDIF

   RETURN Eval(bValidator, xValue)

/**
 * Adds a validation error
 * method _AddError
 * private
 * param {String} cField - Field name with error
 * param {String} cMessage - Error message
 * returns {HbORMValidator} Self for method chaining
*/
METHOD _AddError(cField, cMessage) CLASS HbORMValidator
   AAdd(::aErrors, {cField, cMessage})
   RETURN Self
