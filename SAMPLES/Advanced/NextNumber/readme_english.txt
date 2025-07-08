Not all RDDs support auto-incremental fields, so this function allows you to increment the value of the primary key entry by 1, whether it is a number or character type.

The example detects the maximum number that it can contain so that it does not throw an error due to exceeding the field width.

The nextnumber.prg file explains in detail how the function is used.

Increments the value of a key field in the target database.
It can be character or numeric.
Useful for increasing invoice numbers, etc.

Marcos Jarrin Pita
email  : marvijarrin@gmail.com
website: badasystem.org

