//
                                     
#define    _HMG_OUTLOG

#include "hmg.ch" 
#include "tsbrowse.ch" 

#define    BASE_COLUMNS   "o_Cols"

#translate sColsPrivate()                 => __mvPrivate( BASE_COLUMNS ) ; _CrtCols( BASE_COLUMNS )
#translate sCols( <Key>, <oCol> )         => _SetCols( <"Key">, <oCol> ) 
#translate sCols( <Key>, <Name>, <xVal> ) => _SetCols( <"Key">, <"Name">, <xVal> )
#translate dCols( <Key> )                 => _DelCols( <"Key"> )
#translate gCols( <Key> )                 => _GetCols( <"Key"> )
#translate gCols()                        => _GetCols()

