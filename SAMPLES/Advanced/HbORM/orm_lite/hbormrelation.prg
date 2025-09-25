/*

 BadaSystem
 Program       : HbORM - Harbour Object-Relational Mapping
 Module        : HbORM Relation - Extension for managing relationships between tables
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrin
 Email         : marvijarrin@gmail.com
 Date          : 15/07/2025
 Update        : 05/08/2025
 Rev           : 0.1
 Description: Plugin for HbORM that allows you to manage relationships between tables

*/

#include "hbclass.ch"

/**
 * Class for managing table relationships in HbORM
 * class HbORMRelation
 * description Handles 1:1, 1:N, and N:M relationships between ORM-managed tables
 */
CREATE CLASS HbORMRelation
   EXPORTED:
      VAR oParent           /* Parent ORM object */
      VAR oChild            /* Child ORM object */
      VAR cType             /* Relationship type: "1:1", "1:N", "N:M" */
      VAR cParentKey        /* Key field in parent table */
      VAR cChildKey         /* Key field in child table */
      VAR cJoinTable        /* Join table name (for N:M relationships) */
      VAR cJoinParentKey    /* Join table field referencing parent */
      VAR cJoinChildKey     /* Join table field referencing child */

      /**
      * Class constructor
      */
      METHOD New(oParent, oChild, cType, cParentKey, cChildKey, cJoinTable, cJoinParentKey, cJoinChildKey) CONSTRUCTOR
      METHOD GetRelated(xParentKey)               /* Gets related records for a parent key */
      METHOD SetRelated(xParentKey, aChildData)   /* Sets related records (replaces existing relationships) */
      METHOD AddRelated(xParentKey, hChildData)   /* Adds a related record    */
      METHOD RemoveRelated(xParentKey, xChildKey) /* Removes a relationship   */
      METHOD GetParentByChild(xChildKey)          /* Gets parent record from child key     */

   PROTECTED:

      METHOD _ValidateRelation()   /* Validates relationship configuration */
      METHOD _GetJoinORM()         /* Gets ORM instance for join table (N:M relationships) */
ENDCLASS

/*
 * Constructor method
 * method New
 * param {HbORM} oParent - Parent ORM object
 * param {HbORM} oChild - Child ORM object
 * param {String} cType - Relationship type ("1:1", "1:N", "N:M")
 * param {String} cParentKey - Parent table key field
 * param {String} cChildKey - Child table key field
 * param {String} [cJoinTable] - Join table name (for N:M)
 * param {String} [cJoinParentKey] - Join table parent reference (for N:M)
 * param {String} [cJoinChildKey] - Join table child reference (for N:M)
 * returns {HbORMRelation} New relationship instance or NIL if invalid
 */
METHOD New(oParent, oChild, cType, cParentKey, cChildKey, cJoinTable, cJoinParentKey, cJoinChildKey) CLASS HbORMRelation
   ::oParent        := oParent
   ::oChild         := oChild
   ::cType          := Upper(cType)
   ::cParentKey     := cParentKey
   ::cChildKey      := cChildKey
   ::cJoinTable     := cJoinTable
   ::cJoinParentKey := cJoinParentKey
   ::cJoinChildKey  := cJoinChildKey

   IF !::_ValidateRelation()
      ? "ERROR: Invalid relationship configuration"
      RETURN NIL
   ENDIF

   RETURN Self

/**
* Validates relationship configuration
* method _ValidateRelation
* private
* returns {Boolean} .T. if valid, .F. otherwise
*/
METHOD _ValidateRelation() CLASS HbORMRelation
   // Verify ORM objects
   IF ::oParent == NIL .OR. ::oChild == NIL
      RETURN .F.
   ENDIF

   // Verify relationship type
   IF !(::cType $ "1:1,1:N,N:M")
      RETURN .F.
   ENDIF

   // Verify key fields
   IF Empty(::cParentKey) .OR. Empty(::cChildKey)
      RETURN .F.
   ENDIF

   // For N:M relationships, verify join table configuration
   IF ::cType == "N:M"
      IF Empty(::cJoinTable) .OR. Empty(::cJoinParentKey) .OR. Empty(::cJoinChildKey)
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

/**
 * Gets ORM instance for join table (N:M relationships)
 * method _GetJoinORM
 * private
 * returns {HbORM} ORM instance for join table or NIL
*/
METHOD _GetJoinORM() CLASS HbORMRelation
   LOCAL oJoinORM

   IF ::cType != "N:M" .OR. Empty(::cJoinTable)
      RETURN NIL
   ENDIF

   oJoinORM := HbORM():New(::cJoinTable)
   oJoinORM:Open()

   RETURN oJoinORM

/**
 * Gets related records for a parent key
 * method GetRelated
 * param {*} [xParentKey] - Parent key value (uses current record if NIL)
 * returns {Array} Array of related child records
 */
METHOD GetRelated(xParentKey) CLASS HbORMRelation
   LOCAL aResult := {}
   LOCAL oJoinORM, hJoin
   LOCAL hCondition

   // Verify tables are open
   IF !::oParent:lOpen .OR. !::oChild:lOpen
      ? "ERROR: Tables must be open"
      RETURN aResult
   ENDIF

   // Use current record key if none specified
   IF xParentKey == NIL
      xParentKey := ::oParent:GetValue(::cParentKey)
   ENDIF

   DO CASE
      // One-to-one or one-to-many relationship
      CASE ::cType $ "1:1,1:N"
         ::oChild:GoTop()

         DO WHILE !::oChild:Eof()
            IF ::oChild:GetValue(::cChildKey) == xParentKey
               // For 1:1, return only the first record
               IF ::cType == "1:1"
                  RETURN {::oChild:GetRow()}
               ENDIF

               // For 1:N, add all related records
               AAdd(aResult, ::oChild:GetRow())
            ENDIF

            ::oChild:Skip()
         ENDDO

      // Many-to-many relationship
      CASE ::cType == "N:M"
         oJoinORM := ::_GetJoinORM()

         IF oJoinORM == NIL
            RETURN aResult
         ENDIF

         // Search join table
         oJoinORM:GoTop()

         DO WHILE !oJoinORM:Eof()
            IF oJoinORM:GetValue(::cJoinParentKey) == xParentKey
               // Find corresponding child record
               IF ::oChild:Find(oJoinORM:GetValue(::cJoinChildKey))
                  AAdd(aResult, ::oChild:GetRow())
               ENDIF
            ENDIF

            oJoinORM:Skip()
         ENDDO

         oJoinORM:Close()
   ENDCASE

   RETURN aResult

/**
* Sets related records (replaces existing relationships)
* method SetRelated
* param {*} [xParentKey] - Parent key value (uses current record if NIL)
* param {Array} aChildData - Array of child records to relate
* returns {Boolean} .T. if successful, .F. otherwise
*/
METHOD SetRelated(xParentKey, aChildData) CLASS HbORMRelation
   LOCAL hChild
   LOCAL oJoinORM
   LOCAL lSuccess := .T.

   // Verify tables are open
   IF !::oParent:lOpen .OR. !::oChild:lOpen
      ? "ERROR: Tables must be open"
      RETURN .F.
   ENDIF

   // Use current record key if none specified
   IF xParentKey == NIL
      xParentKey := ::oParent:GetValue(::cParentKey)
   ENDIF

   // Remove existing relationships
   DO CASE
      // One-to-one or one-to-many relationship
      CASE ::cType $ "1:1,1:N"
         ::oChild:GoTop()

         DO WHILE !::oChild:Eof()
            IF ::oChild:GetValue(::cChildKey) == xParentKey
               ::oChild:Delete()
            ENDIF

            ::oChild:Skip()
         ENDDO

         // Insert new related records
         FOR EACH hChild IN aChildData
            hChild[::cChildKey] := xParentKey
            lSuccess := lSuccess .AND. ::oChild:Insert(hChild)
         NEXT

      // Many-to-many relationship
      CASE ::cType == "N:M"
         oJoinORM := ::_GetJoinORM()

         IF oJoinORM == NIL
            RETURN .F.
         ENDIF

         // Remove existing relationships from join table
         oJoinORM:GoTop()

         DO WHILE !oJoinORM:Eof()
            IF oJoinORM:GetValue(::cJoinParentKey) == xParentKey
               oJoinORM:Delete()
            ENDIF

            oJoinORM:Skip()
         ENDDO

         // Insert new relationships
         FOR EACH hChild IN aChildData
            // Ensure child record exists
            IF !::oChild:Find(hChild[::cChildKey])
               ::oChild:Insert(hChild)
            ENDIF

            // Create relationship in join table
            lSuccess := lSuccess .AND. oJoinORM:Insert({::cJoinParentKey => xParentKey, ;
               ::cJoinChildKey => hChild[::cChildKey] })
         NEXT

         oJoinORM:Close()
   ENDCASE

   RETURN lSuccess

/**
 * Adds a related record
 * method AddRelated
 * param {*} [xParentKey] - Parent key value (uses current record if NIL)
 * param {Hash} hChildData - Child record data to relate
 * returns {Boolean} .T. if successful, .F. otherwise
 */
METHOD AddRelated(xParentKey, hChildData) CLASS HbORMRelation
   LOCAL oJoinORM
   LOCAL xChildKey
   LOCAL lSuccess

   // Verify tables are open
   IF !::oParent:lOpen .OR. !::oChild:lOpen
      ? "ERROR: Tables must be open"
      RETURN .F.
   ENDIF

   // Use current record key if none specified
   IF xParentKey == NIL
      xParentKey := ::oParent:GetValue(::cParentKey)
   ENDIF

   DO CASE
      // One-to-one relationship
      CASE ::cType == "1:1"
         // Check if relationship already exists
         ::oChild:GoTop()

         DO WHILE !::oChild:Eof()
            IF ::oChild:GetValue(::cChildKey) == xParentKey
               ? "ERROR: One-to-one relationship already exists"
               RETURN .F.
            ENDIF

            ::oChild:Skip()
         ENDDO

         // Add the relationship
         hChildData[::cChildKey] := xParentKey
         RETURN ::oChild:Insert(hChildData)

      // One-to-many relationship
      CASE ::cType == "1:N"
         hChildData[::cChildKey] := xParentKey
         RETURN ::oChild:Insert(hChildData)

      // Many-to-many relationship
      CASE ::cType == "N:M"
         oJoinORM := ::_GetJoinORM()

         IF oJoinORM == NIL
            RETURN .F.
         ENDIF

         // Ensure child record exists
         xChildKey := hChildData[::cChildKey]

         IF xChildKey == NIL
            // Insert new child record
            IF !::oChild:Insert(hChildData)
               oJoinORM:Close()
               RETURN .F.
            ENDIF

            xChildKey := ::oChild:GetValue(::cChildKey)
         ELSEIF !::oChild:Find(xChildKey)
            // Insert new child record with specific key
            IF !::oChild:Insert(hChildData)
               oJoinORM:Close()
               RETURN .F.
            ENDIF
         ENDIF

         // Check if relationship already exists
         oJoinORM:GoTop()

         DO WHILE !oJoinORM:Eof()
            IF oJoinORM:GetValue(::cJoinParentKey) == xParentKey .AND. ;
               oJoinORM:GetValue(::cJoinChildKey) == xChildKey
               oJoinORM:Close()
               RETURN .T.  // Relationship already exists
            ENDIF

            oJoinORM:Skip()
         ENDDO

         // Create relationship in join table
         lSuccess := oJoinORM:Insert({ ;
            ::cJoinParentKey => xParentKey,;
            ::cJoinChildKey => xChildKey })

         oJoinORM:Close()
         RETURN lSuccess
   ENDCASE

   RETURN .F.

/*
* Removes a relationship
* method RemoveRelated
* param {*} [xParentKey] - Parent key value (uses current record if NIL)
* param {*} xChildKey - Child key value to remove
* returns {Boolean} .T. if successful, .F. otherwise
*/
METHOD RemoveRelated(xParentKey, xChildKey) CLASS HbORMRelation
   LOCAL oJoinORM
   LOCAL lSuccess := .F.

   // Verify tables are open
   IF !::oParent:lOpen .OR. !::oChild:lOpen
      ? "ERROR: Tables must be open"
      RETURN .F.
   ENDIF

   // Use current record key if none specified
   IF xParentKey == NIL
      xParentKey := ::oParent:GetValue(::cParentKey)
   ENDIF

   // Child key must be specified
   IF xChildKey == NIL
      ? "ERROR: Child key must be specified"
      RETURN .F.
   ENDIF

   DO CASE
      // One-to-one or one-to-many relationship
      CASE ::cType $ "1:1,1:N"
         // Find child record
         ::oChild:GoTop()

         DO WHILE !::oChild:Eof()
            IF ::oChild:GetValue(::cChildKey) == xParentKey .AND. ;
               ::oChild:GetValue(::oChild:cTable + "_ID") == xChildKey
               lSuccess := ::oChild:Delete()
               EXIT
            ENDIF

            ::oChild:Skip()
         ENDDO

      // Many-to-many relationship
      CASE ::cType == "N:M"
         oJoinORM := ::_GetJoinORM()

         IF oJoinORM == NIL
            RETURN .F.
         ENDIF

         // Find relationship in join table
         oJoinORM:GoTop()

         DO WHILE !oJoinORM:Eof()
            IF oJoinORM:GetValue(::cJoinParentKey) == xParentKey .AND. ;
               oJoinORM:GetValue(::cJoinChildKey) == xChildKey
               lSuccess := oJoinORM:Delete()
               EXIT
            ENDIF

            oJoinORM:Skip()
         ENDDO

         oJoinORM:Close()
   ENDCASE

   RETURN lSuccess

/*
* Gets parent record from child key
* method GetParentByChild
* param {*} xChildKey - Child key value
* returns {Hash} Parent record data or NIL if not found
*/
METHOD GetParentByChild(xChildKey) CLASS HbORMRelation
   LOCAL oJoinORM
   LOCAL xParentKey

   // Verify tables are open
   IF !::oParent:lOpen .OR. !::oChild:lOpen
      ? "ERROR: Tables must be open"
      RETURN NIL
   ENDIF

   DO CASE
      // One-to-one or one-to-many relationship
      CASE ::cType $ "1:1,1:N"
         // Find child record
         IF ::oChild:Find(xChildKey)
            xParentKey := ::oChild:GetValue(::cChildKey)

            // Find parent record
            IF ::oParent:Find(xParentKey)
               RETURN ::oParent:GetRow()
            ENDIF
         ENDIF

      // Many-to-many relationship
      CASE ::cType == "N:M"
         oJoinORM := ::_GetJoinORM()

         IF oJoinORM == NIL
            RETURN NIL
         ENDIF

         // Search join table
         oJoinORM:GoTop()

         DO WHILE !oJoinORM:Eof()
            IF oJoinORM:GetValue(::cJoinChildKey) == xChildKey
               xParentKey := oJoinORM:GetValue(::cJoinParentKey)

               // Find parent record
               IF ::oParent:Find(xParentKey)
                  oJoinORM:Close()
                  RETURN ::oParent:GetRow()
               ENDIF
            ENDIF
            
            oJoinORM:Skip()
         ENDDO
         
         oJoinORM:Close()
   ENDCASE
   
   RETURN NIL
