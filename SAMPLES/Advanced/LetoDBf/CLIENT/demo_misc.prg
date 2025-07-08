//
#define CaseSensitive      // отключить #define set(...)   LETO_SET(...) !!!

#include "hmg.ch"
#include "tsbrowse.ch" 

*----------------------------------------------------------------------------*
FUNC wPost( nEvent, nIndex, xParam )
*----------------------------------------------------------------------------*
   LOCAL oWnd

   IF HB_ISOBJECT( nIndex )

      IF nIndex:ClassName == 'TSBROWSE'
         oWnd := _WindowObj( nIndex:cParentWnd )
      ELSE
         oWnd := nIndex
      ENDIF

      oWnd:SetProp( nEvent, xParam )
      oWnd:PostMsg( nEvent )

   ELSE

      DEFAULT nEvent := Val( This.Name )

      IF nEvent > 0
         oWnd := ThisWindow.Object
         oWnd:SetProp( nEvent, xParam )
         oWnd:PostMsg( nEvent, nIndex )
      ENDIF

   ENDIF

RETURN NIL

*----------------------------------------------------------------------------*
FUNC wSend( nEvent, nIndex, xParam )
*----------------------------------------------------------------------------*
   LOCAL oWnd

   IF HB_ISOBJECT( nIndex )

      IF nIndex:ClassName == 'TSBROWSE'
         oWnd := _WindowObj( nIndex:cParentWnd )
      ELSE
         oWnd := nIndex
      ENDIF

      oWnd:SetProp( nEvent, xParam )
      oWnd:SendMsg( nEvent )

   ELSE

      DEFAULT nEvent := Val( This.Name )

      IF nEvent > 0
         oWnd := ThisWindow.Object
         oWnd:SetProp( nEvent, xParam )
         oWnd:SendMsg( nEvent, nIndex )
      ENDIF

   ENDIF

RETURN NIL

*----------------------------------------------------------------------------*
FUNC SetsEnv()
*----------------------------------------------------------------------------*

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   rddSetDefault( "DBFCDX" )

   SET CENTURY      ON
   SET DATE         GERMAN
   SET DELETED      ON
   SET EXCLUSIVE    ON
   SET EPOCH TO     2000
   SET AUTOPEN      ON
//   SET EXACT        ON
//   SET SOFTSEEK     ON

   SET NAVIGATION   EXTENDED
   SET FONT         TO "Arial", 11
   SET DEFAULT ICON TO "hmg_ico"

   SET DIALOGBOX CENTER OF PARENT
   
   // --------------------------------
   SET OOP ON
   // --------------------------------

RETURN NIL

*----------------------------------------------------------------------------* 
FUNCTION RecGet() 
*----------------------------------------------------------------------------* 
   LOCAL oRec := oKeyData()

   AEval( Array( FCount() ), {|v,n| v := n, oRec:Set( FieldName( n ), FieldGet( n ) ) } ) 
 
RETURN oRec 
 
*----------------------------------------------------------------------------* 
FUNCTION RecPut( oRec ) 
*----------------------------------------------------------------------------* 
   LOCAL nCnt := 0 
               
   AEval( oRec:GetAll(.F.), {|a,n| n := FieldPos(a[1]), nCnt += n, ; 
                            iif( n > 0, FieldPut( n, a[2] ), ) } ) 
RETURN nCnt > 0
