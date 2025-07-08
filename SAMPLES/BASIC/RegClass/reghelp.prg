
/*
   Some helpful functions for the registry class from
   Scott McNay
*/

Function ReadValue(cKey, cValue)
   LOCAL oReg := XbpReg():NEW( cKey )
   oReg:ReadBinType := "C"
   if empty(cValue)
      return blank(oReg:Standard)
   endif
return blank(oReg:GetValue(cValue))


Function ExistKey(cKey)
   LOCAL oReg := XbpReg():NEW( cKey )
return oReg:status()



Function EnumKey(cKey, Index)
   LOCAL oReg
   STATIC cKeyHold, oRegHold, aValues
   if cKey=cKeyHold
      oReg := oRegHold
   else
      cKeyHold         := cKey
      oReg             := XbpReg():NEW( cKey )
      oReg:ReadBinType := "C"
      oRegHold         := oReg
      aValues          := oReg:keyList()
   endif
   if Index = 0
      return blank(ReadValue(cKey,""))
   elseif Index > 0 .and. Index <= len(aValues)
      return blank(aValues[Index])
   endif
return ""


Function EnumValueKey(cKey, Index)
   LOCAL oReg
   STATIC cKeyHold, oRegHold, aValues
   if cKey=cKeyHold
      oReg := oRegHold
   else
      cKeyHold         := cKey
      oReg             := XbpReg():NEW( cKey )
      oReg:ReadBinType := "C"
      oRegHold         := oReg
      aValues          := oReg:valueList(.F.)
   endif
   if Index = 0
      return blank(ReadValue(cKey,""))
   elseif Index > 0 .and. Index <= len(aValues)
      return blank(aValues[Index])
   endif
return ""



Function EnumValue(cKey, Index)
   LOCAL oReg
   STATIC cKeyHold, oRegHold, aValues
   if cKey=cKeyHold
      oReg := oRegHold
   else
      cKeyHold         := cKey
      oReg             := XbpReg():NEW( cKey )
      oReg:ReadBinType := "C"
      oRegHold         := oReg
      aValues          := oReg:valueList(.T.)
   endif
   if Index = 0
      return blank(ReadValue(cKey,""))
   elseif Index > 0 .and. Index <= len(aValues)
      return blank(aValues[Index,2])
   endif
return ""



static function Blank(x)
   if empty(x)
      return ""
   endif
return x
