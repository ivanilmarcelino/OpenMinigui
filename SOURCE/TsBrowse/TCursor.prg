#ifdef __XHARBOUR__
#define __SYSDATA__
#endif
#include "minigui.ch"
#include "hbclass.ch"

* ============================================================================
CLASS TCursor
* ============================================================================

   DATA   hCursor
   DATA   lPredef AS LOGICAL

   METHOD New( cResName, cPredef ) CONSTRUCTOR

   METHOD End() INLINE If( ::hCursor != 0,;
                       If( ! ::lPredef, DestroyCursor( ::hCursor ),),),;
                           ::hCursor := 0

ENDCLASS

* ============================================================================
METHOD New( cResName, cPredef ) CLASS TCursor
* ============================================================================

   local nAt, aTypes

   if ! Empty( cPredef )
      cPredef := Upper( cPredef )
      if ( nAt := AScan( { "ARROW", "IBEAM", "WAIT",;
                           "CROSS", "UPARROW", ;
                           "SIZENWSE", "SIZENESW",;
                           "SIZEWE", "SIZENS" }, cPredef ) ) != 0

         aTypes = { IDC_ARROW, IDC_IBEAM, IDC_WAIT,;
                    IDC_CROSS, IDC_UPARROW,;
                    IDC_SIZENWSE, IDC_SIZENESW,;
                    IDC_SIZEWE, IDC_SIZENS }

         ::hCursor = LoadCursor( NIL, aTypes[ nAt ] )

         ::lPredef = .T.
      else
         if cPredef == "HAND"
            ::hCursor = GetCursorHand()
            ::lPredef = .F.
         elseif cPredef == "STOP"
            ::hCursor = GetCursorStop()
            ::lPredef = .F.
         elseif cPredef == "DRAG"
            ::hCursor = GetCursorDrag()
            ::lPredef = .F.
         elseif cPredef == "CATCH"
            ::hCursor = GetCursorCatch()
            ::lPredef = .F.
         else
            MsgAlert( "Wrong predefined cursor type!", "Alert" )
         endif
      endif
   else
      ::hCursor = LoadCursor( GetInstance(), cResName )
      ::lPredef = .F.
   endif

Return Self
