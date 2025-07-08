OrdWildSeek()

Searches a value in the controlling index using wild card characters.
Syntax : OrdWildSeek( <cWildCardString>,; 
                      [<lCurrentRec>] , ;
                      [<lBackwards>] ) --> lFound

Arguments :

<cWildCardString> :

This is a character string to search in the controlling index. It may include 
the wild card characters "?" and "*". The question mark matches a single 
character, while the asterisk matches one or more characters.

<lCurrentRec> :

This parameter defaults to .F. (false) causing OrdWildSeek() to begin the 
search with the first record included in the controlling index. When .T. 
(true) is passed, the function begins the search with the current record.

<lBackwards> :

If .T. (true) is passed, OrdWildSeek() searches <cWildCardString> towards 
the begin of file. The default value is .F. (false), i.e. the function 
searches towards the end of file. 

Return :

The function returns .T. (true) if a record matching <cWildCardString> is 
found in the controlling index, otherwise .F. (false) is returned.

Description :

OrdWildSeek() searches a character string that may include wild card 
characters in the controlling index. This allows for collecting subsets 
of records based on an approximate search string. Records matching the 
search string are found in the controlling index, and the record pointer 
is positioned on the found record.

When a matching record is found, the function Found() returns .T. (true) 
until the record pointer is moved again. In addition, both functions, 
BoF() and EoF() return .F. (false).

If the searched value is not found, OrdWildSeek() positions the record 
pointer on the "ghost record" (Lastrec()+1), and the function Found() 
returns .F. (false), while Eof() returns .T. (true). The SET SOFTSEEK 
setting is ignored by OrdWildSeek().
