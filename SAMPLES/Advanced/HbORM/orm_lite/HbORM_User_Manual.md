# HbORM User Manual 📖

Welcome to the **HbORM User Manual**, a comprehensive guide to using the Harbour Object-Relational Mapping (ORM) library for managing DBF/CDX tables. This manual covers the core classes (`HbORM`, `HbORMQuery`, `HbORMValidator`, `HbORMRelation`) and utility functions (`ArrayToList`, `ValToStr`) from the provided code, offering detailed explanations, practical examples, and usage guidance for both beginner and advanced Harbour developers. 🚀

## Table of Contents 📋

1. [Introduction](#introduction)
2. [HbORM Class](#hborm-class)
   - [Purpose](#hborm-purpose)
   - [Attributes](#hborm-attributes)
   - [Methods](#hborm-methods)
3. [HbORMQuery Class](#hbormquery-class)
   - [Purpose](#hbormquery-purpose)
   - [Attributes](#hbormquery-attributes)
   - [Methods](#hbormquery-methods)
4. [HbORMValidator Class](#hbormvalidator-class)
   - [Purpose](#hbormvalidator-purpose)
   - [Attributes](#hbormvalidator-attributes)
   - [Methods](#hbormvalidator-methods)
5. [HbORMRelation Class](#hbormrelation-class)
   - [Purpose](#hbormrelation-purpose)
   - [Attributes](#hbormrelation-attributes)
   - [Methods](#hbormrelation-methods)
6. [Utility Functions](#utility-functions)
   - [ArrayToList](#arraytolist)
   - [ValToStr](#valtostr)
7. [Getting Started](#getting-started)
8. [Best Practices](#best-practices)

---

## Introduction

HbORM is a lightweight Object-Relational Mapping library designed for Harbour, enabling developers to interact with DBF/CDX tables using an object-oriented approach. It simplifies table management, record manipulation, query building, data validation, and relationship handling. The library includes:

- **HbORM**: Core class for DBF table operations.
- **HbORMQuery**: Fluent interface for building complex queries.
- **HbORMValidator**: Data validation for ORM models.
- **HbORMRelation**: Manages relationships (1:1, 1:N, N:M) between tables.
- **Utility Functions**: Helper functions for string conversion and array handling.

This manual assumes familiarity with Harbour and DBF/CDX databases. 📌

---

## HbORM Class

### HbORM Purpose 📋

The `HbORM` class is the foundation of the HbORM library, providing methods to manage DBF tables, including opening, creating, indexing, and manipulating records. It abstracts low-level database operations into a simple, object-oriented interface.

### HbORM Attributes 🔍

| Attribute     | Type   | Purpose                                                                 |
|---------------|--------|-------------------------------------------------------------------------|
| `cTable`      | String | Name of the DBF table file (without extension).                         |
| `cAlias`      | String | Alias for the table, used for database operations.                      |
| `aStructure`  | Array  | Stores the table's structure (field names, types, lengths, etc.).       |
| `lOpen`       | Logical| Indicates whether the table is open (`.T.`) or closed (`.F.`).          |
| `cPath`       | String | Path to the directory containing the table.                             |
| `aIndexes`    | Array  | Stores index information (tag, key, condition, uniqueness).             |
| `cError`      | String | Stores the last error message (protected).                              |

### HbORM Methods 🛠️

#### New(cTable, cAlias, cPath)

**Description**: Initializes an `HbORM` object for a specific DBF table.

**Parameters**:
- `cTable` (String): Table name (without `.dbf`).
- `cAlias` (String, optional): Table alias; defaults to `cTable`.
- `cPath` (String, optional): Directory path; defaults to current directory.

**Returns**: `HbORM` object.

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers", "CUST", "C:\data\")
? oORM:cTable  // Outputs: customers
? oORM:cAlias  // Outputs: CUST
? oORM:cPath   // Outputs: C:\data\
```

#### Exists()

**Description**: Checks if the DBF table file exists.

**Returns**: Logical (`.T.` if exists, `.F.` otherwise).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
IF oORM:Exists()
   ? "Table exists! ✅"
ELSE
   ? "Table not found: " + oORM:GetError()
ENDIF
```

#### Open(lShared)

**Description**: Opens the DBF table in shared or exclusive mode.

**Parameters**:
- `lShared` (Logical, optional): `.T.` for shared mode (default), `.F.` for exclusive.

**Returns**: Logical (`.T.` on success, `.F.` on failure).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
IF oORM:Open()
   ? "Table opened successfully! ✅"
ELSE
   ? "Error: " + oORM:GetError()
ENDIF
```

#### Close()

**Description**: Closes the open DBF table.

**Returns**: Logical (`.T.` on success, `.F.` if not open).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:Close()
? oORM:lOpen  // Outputs: .F.
```

#### Create(aStruct)

**Description**: Creates a new DBF table with the specified structure.

**Parameters**:
- `aStruct` (Array): Table structure (e.g., `{{"ID", "N", 10, 0}, {"NAME", "C", 50, 0}}`).

**Returns**: Logical (`.T.` on success, `.F.` on failure).

**Example**:
```harbour
LOCAL oORM := HbORM():New("products")
LOCAL aStruct := {{"ID", "N", 10, 0}, {"NAME", "C", 50, 0}}
IF oORM:Create(aStruct)
   ? "Table created! ✅"
ELSE
   ? "Error: " + oORM:GetError()
ENDIF
```

#### AddIndex(cTag, cKey, cFor, lUnique)

**Description**: Adds an index to the table.

**Parameters**:
- `cTag` (String): Index tag name.
- `cKey` (String): Key expression (e.g., `"ID"`).
- `cFor` (String, optional): FOR condition.
- `lUnique` (Logical, optional): `.T.` for unique index (default `.F.`).

**Returns**: Logical (`.T.` on success, `.F.` on failure).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:AddIndex("IDX_ID", "ID", "", .T.)
? "Index added! ✅"
```

#### OpenIndexes()

**Description**: Opens all indexes associated with the table.

**Returns**: Logical (`.T.` on success, `.F.` if not open or no indexes).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
IF oORM:OpenIndexes()
   ? "Indexes opened! ✅"
ENDIF
```

#### Find(xKey, cTag, lLast ) / Seek(xKey, cTag, lLast  )

**Description**: Searches for a record by key value in the specified index.

**Parameters**:
- `xKey` (Any): Key value to search.
- `cTag` (String, optional): Index tag.
- `lLast`(Logical, optional): Last of multiple records having the same index value.

**Returns**: Logical (`.T.` if found, `.F.` otherwise).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
IF oORM:Find(1001, "IDX_ID")
   ? "Record found! ✅"
ELSE
   ? "Record not found."
ENDIF
```

#### Skip(nRecords)

**Description**: Moves the record pointer by the specified number of records.

**Parameters**:
- `nRecords` (Numeric, optional): Records to skip (default 1).

**Returns**: Logical (`.T.` on success, `.F.` if not open).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:GoTop()
oORM:Skip(2)  // Move forward 2 records
? oORM:RecNo()  // Outputs current record number
```

#### GoTo(nRecord)

**Description**: Moves to the specified record number.

**Parameters**:
- `nRecord` (Numeric): Record number.

**Returns**: Logical (`.T.` on success, `.F.` if not open).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:GoTo(5)
? oORM:RecNo()  // Outputs: 5
```

#### GoTop()

**Description**: Moves to the first record.

**Returns**: Logical (`.T.` on success, `.F.` if not open).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:GoTop()
? oORM:RecNo()  // Outputs: 1
```

#### GoBottom()

**Description**: Moves to the last record.

**Returns**: Logical (`.T.` on success, `.F.` if not open).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:GoBottom()
? oORM:RecNo()  // Outputs: last record number
```

#### RecCount()

**Description**: Returns the total number of records.

**Returns**: Numeric (number of records, 0 if not open).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
? oORM:RecCount()  // Outputs: total records
```

#### RecNo()

**Description**: Returns the current record number.

**Returns**: Numeric (current record number, 0 if not open).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:GoTop()
? oORM:RecNo()  // Outputs: 1
```

#### Eof()

**Description**: Checks if the record pointer is at the end of the table.

**Returns**: Logical (`.T.` if at end or not open, `.F.` otherwise).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:GoBottom()
oORM:Skip()
? oORM:Eof()  // Outputs: .T.
```

#### Bof()

**Description**: Checks if the record pointer is at the beginning of the table.

**Returns**: Logical (`.T.` if at beginning or not open, `.F.` otherwise).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:GoTop()
? oORM:Bof()  // Outputs: .T.
```

#### GetValue(cField)

**Description**: Retrieves the value of a specified field in the current record.

**Parameters**:
- `cField` (String): Field name.

**Returns**: Any (field value, `NIL` if not open or field doesn't exist).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:GoTop()
? oORM:GetValue("NAME")  // Outputs: customer name
```

#### GetRow()

**Description**: Retrieves all field values of the current record as a hash.

**Returns**: Hash (field names and values, `NIL` if no current record).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:GoTop()
LOCAL hRow := oORM:GetRow()
? hRow["NAME"]  // Outputs: customer name
```

#### SetValue(cField, xValue)

**Description**: Sets the value of a field in the current record.

**Parameters**:
- `cField` (String): Field name.
- `xValue` (Any): Value to set.

**Returns**: Logical (`.T.` on success, `.F.` on failure).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:GoTop()
IF oORM:SetValue("NAME", "John Doe")
   ? "Field updated! ✅"
ENDIF
```

#### GetStruct()

**Description**: Returns the table's structure.

**Returns**: Array (table structure).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
? ValToStr(oORM:GetStruct())  // Outputs: table structure
```

#### Insert(hData)

**Description**: Inserts a new record with the provided data.

**Parameters**:
- `hData` (Hash): Field names and values to insert.

**Returns**: Logical (`.T.` on success, `.F.` on failure).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
LOCAL hData := { "ID" => 1001, "NAME" => "John Doe" }
IF oORM:Insert(hData)
   ? "Record inserted! ✅"
ENDIF
```

#### Update(hData)

**Description**: Updates the current record with the provided data.

**Parameters**:
- `hData` (Hash): Field names and values to update.

**Returns**: Logical (`.T.` on success, `.F.` on failure).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:Find(1001)
LOCAL hData := { "NAME" => "Jane Doe" }
IF oORM:Update(hData)
   ? "Record updated! ✅"
ENDIF
```

#### Delete()

**Description**: Marks the current record as deleted.

**Returns**: Logical (`.T.` on success, `.F.` on failure).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
oORM:Find(1001)
IF oORM:Delete()
   ? "Record marked for deletion! ✅"
ENDIF
```

#### Pack()

**Description**: Physically removes deleted records from the table.

**Returns**: Logical (`.T.` on success, `.F.` if not open).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
IF oORM:Pack()
   ? "Table packed! ✅"
ENDIF
```

#### Zap()

**Description**: Deletes all records from the table.

**Returns**: Logical (`.T.` on success, `.F.` if not open).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
IF oORM:Zap()
   ? "Table cleared! ✅"
ENDIF
```

#### GetError()

**Description**: Retrieves the last error message.

**Returns**: String (error message).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
IF !oORM:Open()
   ? oORM:GetError()  // Outputs: error message
ENDIF
```

#### SetOrder(cOrderField)

**Description**: Sets the record order using an existing or new index.

**Parameters**:
- `cOrderField` (String): Field to order by.

**Returns**: Logical (`.T.` on success, `.F.` if not open).

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
oORM:Open()
IF oORM:SetOrder("NAME")
   ? "Table ordered by NAME! ✅"
ENDIF
```

---

## HbORMQuery Class

### HbORMQuery Purpose 📋

The `HbORMQuery` class provides a fluent interface for building and executing complex queries on `HbORM` tables, supporting field selection, conditions, joins, ordering, and limits.

### HbORMQuery Attributes 🔍

| Attribute      | Type   | Purpose                                                                 |
|----------------|--------|-------------------------------------------------------------------------|
| `oORM`         | Object | Target `HbORM` object for query execution.                              |
| `aConditions`  | Array  | Stores WHERE conditions for the query.                                  |
| `aFields`      | Array  | Fields to select in the query.                                         |
| `cOrder`       | String | Field to order results by.                                             |
| `lDesc`        | Logical| Indicates descending order (`.T.`) or ascending (`.F.`).               |
| `nLimit`       | Numeric| Maximum number of records to return.                                   |
| `aJoins`       | Array  | Stores join definitions (type, table, alias, condition).                |
| `cError`       | String | Last error message.                                                    |

### HbORMQuery Methods 🛠️

#### New(oORM)

**Description**: Initializes a query builder for an `HbORM` object.

**Parameters**:
- `oORM` (HbORM): Target ORM object.

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
LOCAL oORM := HbORM():New("customers")
LOCAL oQuery := HbORMQuery():New(oORM)
```

#### Select(aFields)

**Description**: Specifies fields to select in the query.

**Parameters**:
- `aFields` (Array/String): Fields to select (array or single field).

**Returns**: `HbORMQuery` object (for chaining).

**Example**:
```harbour
LOCAL oQuery := HbORMQuery():New(HbORM():New("customers"))
oQuery:Select({"ID", "NAME"})
```

#### Where(cField, cOperator, xValue)

**Description**: Adds a WHERE condition with AND logic.

**Parameters**:
- `cField` (String): Field name.
- `cOperator` (String): Comparison operator (e.g., `=`, `>`, `<`).
- `xValue` (Any): Comparison value.

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
oQuery:Where("AGE", ">", 25)
```

#### OrWhere(cField, cOperator, xValue)

**Description**: Adds a WHERE condition with OR logic.

**Parameters**: Same as `Where`.

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
oQuery:OrWhere("CITY", "=", "New York")
```

#### WhereIn(cField, aValues)

**Description**: Adds a WHERE IN condition.

**Parameters**:
- `cField` (String): Field name.
- `aValues` (Array): Values to match.

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
oQuery:WhereIn("ID", {1001, 1002, 1003})
```

#### WhereNotIn(cField, aValues)

**Description**: Adds a WHERE NOT IN condition.

**Parameters**: Same as `WhereIn`.

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
oQuery:WhereNotIn("ID", {999, 1000})
```

#### WhereBetween(cField, xValue1, xValue2)

**Description**: Adds a WHERE BETWEEN condition.

**Parameters**:
- `cField` (String): Field name.
- `xValue1` (Any): Lower bound.
- `xValue2` (Any): Upper bound.

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
oQuery:WhereBetween("AGE", 18, 30)
```

#### WhereNull(cField)

**Description**: Adds a WHERE IS NULL condition.

**Parameters**:
- `cField` (String): Field name.

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
oQuery:WhereNull("EMAIL")
```

#### WhereNotNull(cField)

**Description**: Adds a WHERE IS NOT NULL condition.

**Parameters**:
- `cField` (String): Field name.

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
oQuery:WhereNotNull("NAME")
```

#### OrderBy(cField, lDesc)

**Description**: Specifies result ordering.

**Parameters**:
- `cField` (String): Field to order by.
- `lDesc` (Logical, optional): `.T.` for descending (default `.F.`).

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
oQuery:OrderBy("NAME", .T.)
```

#### Limit(nLimit)

**Description**: Sets the maximum number of records to return.

**Parameters**:
- `nLimit` (Numeric): Record limit.

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
oQuery:Limit(10)
```

#### Join(cTable, cAlias, cCondition, cType)

**Description**: Adds a table join.

**Parameters**:
- `cTable` (String): Table to join.
- `cAlias` (String): Table alias.
- `cCondition` (String): Join condition.
- `cType` (String, optional): Join type (default `"INNER"`).

**Returns**: `HbORMQuery` object.

**Example**:
```harbour
oQuery:Join("orders", "ORD", "CUST.ID = ORD.CUST_ID", "LEFT")
```

#### Get()

**Description**: Executes the query and returns matching records.

**Returns**: Array of hash records.

**Example**:
```harbour
LOCAL oQuery := HbORMQuery():New(HbORM():New("customers"))
oQuery:Where("AGE", ">", 25):Limit(5)
LOCAL aResults := oQuery:Get()
FOR EACH hRow IN aResults
   ? hRow["NAME"]
NEXT
```

#### First()

**Description**: Returns the first matching record.

**Returns**: Hash (first record, `NIL` if none).

**Example**:
```harbour
LOCAL hRow := oQuery:Where("ID", "=", 1001):First()
IF hRow != NIL
   ? hRow["NAME"]  // Outputs: customer name
ENDIF
```

#### Count()

**Description**: Counts matching records.

**Returns**: Numeric (number of records).

**Example**:
```harbour
? oQuery:Where("CITY", "=", "New York"):Count()  // Outputs: count
```

#### ToSQL()

**Description**: Generates the SQL equivalent of the query (for debugging).

**Returns**: String (SQL query).

**Example**:
```harbour
oQuery:Select({"ID", "NAME"}):Where("AGE", ">", 25)
? oQuery:ToSQL()  // Outputs: SELECT ID, NAME FROM customers WHERE AGE > 25
```

#### GetError()

**Description**: Retrieves the last error message.

**Returns**: String (error message).

**Example**:
```harbour
IF !oQuery:Get()
   ? oQuery:GetError()
ENDIF
```

---

## HbORMValidator Class

### HbORMValidator Purpose 📋

The `HbORMValidator` class provides robust data validation for ORM models, supporting various validation rules such as required fields, type checking, and custom validations.

### HbORMValidator Attributes 🔍

| Attribute  | Type  | Purpose                                                  |
|------------|-------|----------------------------------------------------------|
| `aRules`   | Array | Stores validation rules (field, type, value, message).    |
| `aErrors`  | Array | Stores validation errors (field, message).               |

### HbORMValidator Methods 🛠️

#### New()

**Description**: Initializes a validator instance.

**Returns**: `HbORMValidator` object.

**Example**:
```harbour
LOCAL oValidator := HbORMValidator():New()
```

#### AddRule(cField, cType, xValue, cMessage)

**Description**: Adds a validation rule.

**Parameters**:
- `cField` (String): Field name.
- `cType` (String): Validation type (`required`, `type`, `min`, `max`, `length`, `regex`, `email`, `date`, `custom`).
- `xValue` (Any): Validation parameter (depends on type).
- `cMessage` (String): Error message if validation fails.

**Returns**: `HbORMValidator` object.

**Example**:
```harbour
oValidator:AddRule("EMAIL", "email", NIL, "Invalid email format")
oValidator:AddRule("AGE", "min", 18, "Must be at least 18")
```

#### Validate(hData)

**Description**: Validates data against defined rules.

**Parameters**:
- `hData` (Hash): Field-value pairs to validate.

**Returns**: Logical (`.T.` if valid, `.F.` if errors).

**Example**:
```harbour
LOCAL hData := { "EMAIL" => "user@example.com", "AGE" => 25 }
IF oValidator:Validate(hData)
   ? "Data is valid! ✅"
ELSE
   ? oValidator:GetErrors()
ENDIF
```

#### GetErrors()

**Description**: Returns all validation errors.

**Returns**: Array of `{field, message}` pairs.

**Example**:
```harbour
LOCAL aErrors := oValidator:GetErrors()
FOR EACH aError IN aErrors
   ? aError[1] + ": " + aError[2]
NEXT
```

#### HasErrors()

**Description**: Checks if validation errors exist.

**Returns**: Logical (`.T.` if errors, `.F.` otherwise).

**Example**:
```harbour
IF oValidator:HasErrors()
   ? "Validation failed!"
ENDIF
```

#### ClearErrors()

**Description**: Clears all validation errors.

**Returns**: `HbORMValidator` object.

**Example**:
```harbour
oValidator:ClearErrors()
? oValidator:HasErrors()  // Outputs: .F.
```

---

## HbORMRelation Class

### HbORMRelation Purpose 📋

The `HbORMRelation` class manages relationships (1:1, 1:N, N:M) between `HbORM` tables, enabling operations like retrieving, setting, adding, and removing related records.

### HbORMRelation Attributes 🔍

| Attribute        | Type   | Purpose                                                         |
|------------------|--------|-----------------------------------------------------------------|
| `oParent`        | Object | Parent `HbORM` object.                                          |
| `oChild`         | Object | Child `HbORM` object.                                           |
| `cType`          | String | Relationship type (`1:1`, `1:N`, `N:M`).                        |
| `cParentKey`     | String | Key field in parent table.                                      |
| `cChildKey`      | String | Key field in child table.                                       |
| `cJoinTable`     | String | Join table name (for N:M relationships).                        |
| `cJoinParentKey` | String | Join table field referencing parent (for N:M).                  |
| `cJoinChildKey`  | String | Join table field referencing child (for N:M).                   |

### HbORMRelation Methods 🛠️

#### New(oParent, oChild, cType, cParentKey, cChildKey, cJoinTable, cJoinParentKey, cJoinChildKey)

**Description**: Initializes a relationship between two tables.

**Parameters**:
- `oParent` (HbORM): Parent ORM object.
- `oChild` (HbORM): Child ORM object.
- `cType` (String): Relationship type (`1:1`, `1:N`, `N:M`).
- `cParentKey` (String): Parent table key field.
- `cChildKey` (String): Child table key field.
- `cJoinTable` (String, optional): Join table name (N:M).
- `cJoinParentKey` (String, optional): Join table parent reference (N:M).
- `cJoinChildKey` (String, optional): Join table child reference (N:M).

**Returns**: `HbORMRelation` object or `NIL` if invalid.

**Example**:
```harbour
LOCAL oParent := HbORM():New("customers")
LOCAL oChild := HbORM():New("orders")
LOCAL oRelation := HbORMRelation():New(oParent, oChild, "1:N", "CUST_ID", "CUST_ID")
```

#### GetRelated(xParentKey)

**Description**: Retrieves related child records for a parent key.

**Parameters**:
- `xParentKey` (Any, optional): Parent key value (uses current record if `NIL`).

**Returns**: Array of child records.

**Example**:
```harbour
oParent:Open()
oChild:Open()
LOCAL aOrders := oRelation:GetRelated(1001)
FOR EACH hOrder IN aOrders
   ? hOrder["ORDER_ID"]
NEXT
```

#### SetRelated(xParentKey, aChildData)

**Description**: Replaces existing relationships with new child records.

**Parameters**:
- `xParentKey` (Any, optional): Parent key value.
- `aChildData` (Array): Array of child records to relate.

**Returns**: Logical (`.T.` on success, `.F.` on failure).

**Example**:
```harbour
LOCAL aOrders := { { "ORDER_ID" => 1, "AMOUNT" => 100 }, { "ORDER_ID" => 2, "AMOUNT" => 200 } }
IF oRelation:SetRelated(1001, aOrders)
   ? "Relationships set! ✅"
ENDIF
```

#### AddRelated(xParentKey, hChildData)

**Description**: Adds a single related child record.

**Parameters**:
- `xParentKey` (Any, optional): Parent key value.
- `hChildData` (Hash): Child record data.

**Returns**: Logical (`.T.` on success, `.F.` on failure).

**Example**:
```harbour
LOCAL hOrder := { "ORDER_ID" => 3, "AMOUNT" => 300 }
IF oRelation:AddRelated(1001, hOrder)
   ? "Related record added! ✅"
ENDIF
```

#### RemoveRelated(xParentKey, xChildKey)

**Description**: Removes a specific relationship.

**Parameters**:
- `xParentKey` (Any, optional): Parent key value.
- `xChildKey` (Any): Child key value to remove.

**Returns**: Logical (`.T.` on success, `.F.` on failure).

**Example**:
```harbour
IF oRelation:RemoveRelated(1001, 3)
   ? "Relationship removed! ✅"
ENDIF
```

#### GetParentByChild(xChildKey)

**Description**: Retrieves the parent record for a given child key.

**Parameters**:
- `xChildKey` (Any): Child key value.

**Returns**: Hash (parent record, `NIL` if not found).

**Example**:
```harbour
LOCAL hCustomer := oRelation:GetParentByChild(3)
IF hCustomer != NIL
   ? hCustomer["NAME"]
ENDIF
```

---

## Utility Functions

### ArrayToList(aArray, cDelimiter, lQuote)

**Description**: Converts an array to a delimited string.

**Parameters**:
- `aArray` (Array): Array to convert.
- `cDelimiter` (String, optional): Delimiter (default `,`).
- `lQuote` (Logical, optional): Quote elements (default `.F.`).

**Returns**: String (delimited list).

**Example**:
```harbour
LOCAL aFruits := {"apple", "orange", "banana"}
? ArrayToList(aFruits)  // Outputs: apple,orange,banana
? ArrayToList(aFruits, ",", .T.)  // Outputs: 'apple','orange','banana'
```

### ValToStr(xValue)

**Description**: Converts any value to its string representation (private).

**Parameters**:
- `xValue` (Any): Value to convert.

**Returns**: String (string representation).

**Example**:
```harbour
? ValToStr(42)  // Outputs: 42
? ValToStr(.T.)  // Outputs: .T.
? ValToStr(CTOD("2025-08-02"))  // Outputs: 2025-08-02
```

---

## Getting Started 🚀

1. **Initialize HbORM**:
   ```harbour
   LOCAL oORM := HbORM():New("customers", "CUST", "C:\data\")
   oORM:Open()
   ```

2. **Create a Table**:
   ```harbour
   LOCAL aStruct := {{"ID", "N", 10, 0}, {"NAME", "C", 50, 0}}
   oORM:Create(aStruct)
   ```

3. **Insert Data**:
   ```harbour
   oORM:Insert({ "ID" => 1001, "NAME" => "John Doe" })
   ```

4. **Query Data**:
   ```harbour
   LOCAL oQuery := HbORMQuery():New(oORM)
   LOCAL aResults := oQuery:Where("NAME", "=", "John Doe"):Get()
   ```

5. **Validate Data**:
   ```harbour
   LOCAL oValidator := HbORMValidator():New()
   oValidator:AddRule("NAME", "required", NIL, "Name is required")
   oValidator:Validate({ "NAME" => "John Doe" })
   ```

6. **Manage Relationships**:
   ```harbour
   LOCAL oParent := HbORM():New("customers")
   LOCAL oChild := HbORM():New("orders")
   LOCAL oRelation := HbORMRelation():New(oParent, oChild, "1:N", "CUST_ID", "CUST_ID")
   oRelation:AddRelated(1001, { "ORDER_ID" => 1, "AMOUNT" => 100 })
   ```

---

## Best Practices 📌

- **Error Handling**: Always check `GetError()` after operations to handle failures gracefully.
- **Table Management**: Close tables with `Close()` when done to free resources.
- **Validation**: Use `HbORMValidator` before inserting/updating to ensure data integrity.
- **Relationships**: Validate relationship configurations with `_ValidateRelation()` before use.
- **Query Optimization**: Use `Limit()` and specific `Select()` fields to reduce resource usage.

This manual provides a complete guide to using HbORM effectively. For further assistance, contact marvijarrin@gmail.com. Happy coding! 🚀