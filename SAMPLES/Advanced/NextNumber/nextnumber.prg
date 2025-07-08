/*

 BadaSystem
 Program       : nextnumber
 Modulo        : nextnumber
 Compilador    : MINIGUI - Harbour Win32 GUI
 Link          : BCC 32 bit
 Autor         : Marcos Jarrín
 email         : marvijarrin@gmail.com
 website       : badasystem.org
 Date          : 01/05/2010
 Update        : 28/07/2022

*/

FUNCTION NextNumber( cAlias, cFieldName, nIndexOrder )
   //----------------------------------------------------------------------------
   // Function: NextNumber
   // Purpose:  Generates the next number in a sequence for a given field in a database.
   //           Handles both numeric and character fields.  Useful for auto-incrementing IDs,
   //           invoice numbers, etc.
   // Parameters:
   //    cAlias      - The alias of the database to work with (e.g., "Customers").
   //    cFieldName  - The name of the field to increment (e.g., "InvoiceID").
   //    nIndexOrder - (Optional) The index order to use when finding the last record.
   //                  If omitted, the current index order is used.
   // Returns:    The next number in the sequence as a character or numeric value,
   //             depending on the field type.
   //----------------------------------------------------------------------------

   LOCAL nOldOrder := ( cAlias )->( IndexOrd() )
   // Store the current index order of the database.  This is important so we can
   // restore it after we're done.  IndexOrd() returns the current index order number.

   LOCAL nTargetPos := ( cAlias )->( RecNo() )
   // Store the current record number of the database.  Similar to the index order,
   // we need to restore this after we're done. RecNo() returns the current record number.

   LOCAL retvalue
   // Declare a local variable to store the value that will be returned.

   LOCAL size
   // Declare a local variable to store the field size (used for character fields).

   IF nIndexOrder == NIL
      nIndexOrder := 0
   ENDIF
   // If no index order is specified, default to 0 (natural order).  NIL means the parameter
   // was not passed to the function.

   ( cAlias )->( dbSetOrder( nIndexOrder ) )
   // Set the index order of the database to the specified order.  This is necessary to
   // find the *last* record in the desired order. dbSetOrder() changes the active index.

   ( cAlias )->( dbGoBottom() )
   // Move the record pointer to the last record in the database, according to the current
   // index order. dbGoBottom() moves to the last record.

   // fetch value to be incremented
   retvalue := ( cAlias )->( FieldGet( FieldPos( cFieldName ) ) )
   // Get the value of the specified field from the current record (which is the last record).
   // FieldPos() returns the field number of the specified field name.
   // FieldGet() returns the value of the field at the specified field number.

   IF Empty( retvalue )
      // If the field is empty, we need to initialize it.
      IF ValType( retvalue ) == "C"
         // If the field is a character field, initialize it to a string of zeros.
         retvalue := StrZero( 1, Len( retvalue ) )
         // StrZero(1, Len(retvalue)) creates a string of zeros with the same length as the field.
      ELSEIF ValType( retvalue ) == "N"
         // If the field is a numeric field, initialize it to 1.
         retvalue := 1
      ENDIF
   ELSEIF ValType( retvalue ) == "C"
      // If the field is a character field and not empty, increment it as a number,
      // then format it back to a character string with leading zeros.
      size := Len( retvalue )
      // Store the original length of the string.
      retvalue := LTrim( Str( Val( retvalue ) + 1 ) )
      // Convert the string to a number, increment it, convert it back to a string,
      // and remove any leading spaces. Val() converts a string to a number.
      // Str() converts a number to a string. LTrim() removes leading spaces.
      size -= Len( retvalue )
      // Calculate the number of leading zeros needed.
      retvalue := Replicate( "0", size ) + retvalue
      // Add the leading zeros to the string. Replicate() creates a string by repeating
      // a character a specified number of times.
   ELSEIF ValType( retvalue ) == "N"
      // If the field is a numeric field and not empty, simply increment it.
      retvalue++
   ENDIF

   ( cAlias )->( dbGoto( nTargetPos ) )
   // Restore the record pointer to its original position. dbGoto() moves to a specific record number.

   ( cAlias )->( dbSetOrder( nOldOrder ) )
   // Restore the original index order.

RETURN retvalue  // Return the incremented value.
