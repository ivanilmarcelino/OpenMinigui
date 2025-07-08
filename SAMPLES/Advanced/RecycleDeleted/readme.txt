Feature of recycling of deleted records

We are aware that huge accumulation of deleted records in a dbf greatly reduces performance.
Therefore we introduces the feature of recycling of deleted records in DBF opened via "DBFCDX".

This feature is available for dbfs opened via "DBFCDX" only and it is possible by using
the following new function:

DBFAPPEND( [param] ) --> nAppendedOrRecyledRecNo

where a param may be:
1. Omitted. It is equal to .T.
2. .T.. If set to .t., DBFAPPEND() will always try to recycle deleted records.
3. .F.. If set to .f., DBFAPPEND() does not recycle. Same as DBAPPEND()

Use DBFAPPEND() instead of DBAPPEND() to use this feature.

Present usage of DBAPPEND():

DBAPPEND()
If .not. NetErr()
   // assign values to fields
   DBUNLOCK()
endif
 

Instead, using DBFAPPEND() this way enables using the recycling feature:

if DBFAPPEND() > 0
   // assign values to fields
   DBUNLOCK()
endif


Working of DBFAPPEND()

Step 1: Depending on the setting of DBFAPPEND() parameter, a deleted record that can be locked is located.
If found, the record is locked, data is erased, the record is RECALLed and the record number is returned.

Step 2: If not, attempts to append a new record by trying 4 times at an interval of 0.25 seconds and
returns the newly appended record number if it was successful.

Result of this function is the same as normal DBAPPEND() if the setting is .f.

For the purpose of identifying the deleted records, this feature uses either of two indexes:
a) Any index with for condition "FOR DELETED()"
eg: INDEX ON RECNO() TAG RECYCLE FOR DELETED()
b) Any index with index expression DELETED()
eg: INDEX ON DELETED() TAG DELETED.

Note: Value of the AUTOINC field is not changed when recycling the records.
