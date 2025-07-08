/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
*/

#include "hmg.ch"

////////////////////////////////////////////////////////////////////
// Список объектов на формах / List of objects on forms
FUNCTION myListObjectsForms()
   LOCAL aFrm, nI, nJ, cForm, aHide, aVal, cObj, cType

   aHide := {}
   aFrm  := HMG_GetForms()

   FOR nI := 1 TO LEN(aFrm)
      cForm := UPPER(aFrm[nI])
      IF GetProperty( cForm, "Visible" )  // только НЕ скрытые формы
         aVal := HMG_GetFormControls ( cForm, "ALL" )
         FOR nJ := 1 TO LEN(aVal)
            cObj := aVal[nJ]
            IF GetProperty( cForm, cObj , "Enabled" )  // добавить объект только с Enabled == .T.
               cType := GetControlType ( cObj, cForm )
               AADD( aHide, { cForm, cObj, cType } )
            ENDIF
         NEXT
      ENDIF
   NEXT

RETURN aHide

////////////////////////////////////////////////////////////
// Сохранять\восстанавливать среду This самому
FUNCTION _ThisInfo( aThis )

   IF HB_ISARRAY( aThis )

      _HMG_ThisFormIndex   := aThis[1]
      _HMG_ThisEventType   := aThis[2]
      _HMG_ThisType       := aThis[3]
      _HMG_ThisIndex     := aThis[4]
      _HMG_ThisFormName  := aThis[5]
      _HMG_ThisControlName := aThis[6]

      RETURN NIL

   ENDIF

RETURN { _HMG_ThisFormIndex, _HMG_ThisEventType, _HMG_ThisType, _HMG_ThisIndex,;
                _HMG_ThisFormName, _HMG_ThisControlName }

////////////////////////////////////////////////////////////
FUNCTION To_Focus( cForm, cControl )
   LOCAL hForm, lRet
   DEFAULT cForm   := ""
   DEFAULT cControl := ""

   IF Empty( cForm )
      MsgStop( "Undefined Window ! cForm = NIL !" + ;
            CRLF + CRLF + ProcNL(0) + CRLF + ProcNL(1) )
      RETURN .F.
   ENDIF

   IF ( lRet := _IsWindowDefined(cForm) )
      hForm := GetFormHandle(cForm)
      IF hForm != 0
         IF IsIconic( hForm ) ; _Restore( hForm )
         ENDIF
         SetProperty(cForm, "Topmost", .T.)
         DoMethod(cForm, "SetFocus")
         IF !Empty(cControl) .and. _IsControlDefined(cControl, cForm)
            DoMethod(cForm, cControl, "SetFocus")
         ENDIF
         SetProperty(cForm, "Topmost", .F.)
      ENDIF
   ENDIF

RETURN lRet

///////////////////////////////////////////////////////////////////
FUNCTION myDrawButton(nRow, nCol, nWBtn, nHBtn, aBtn, bAction)

   @ nRow, nCol BUTTONEX &(aBtn[1]) CAPTION aBtn[2] ;
     ICON LoadIconByName( aBtn[3], aBtn[5], aBtn[5] ) ;
     WIDTH nWBtn HEIGHT nHBtn BACKCOLOR aBtn[6] BOLD ;
     NOXPSTYLE HANDCURSOR NOTABSTOP /*VERTICAL*/    ;
     ON MOUSEHOVER ( This.Icon := LoadIconByName( aBtn[4], aBtn[5], aBtn[5] ) ) ;
     ON MOUSELEAVE ( This.Icon := LoadIconByName( aBtn[3], aBtn[5], aBtn[5] ) ) ;
     ON INIT {|| This.Cargo := { aBtn, ThisWindow.Name, This.Name }  }
     //ACTION Eval(bAction) - не надо так

   This.&(aBtn[1]).Icon   := LoadIconByName( aBtn[3], aBtn[5], aBtn[5] )
   This.&(aBtn[1]).Action := bAction

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal)
   DEFAULT nVal := 0
   RETURN " Call from: " + ProcName( nVal + 1 ) + ;
          "(" + hb_ntos( ProcLine( nVal + 1 ) ) + ") --> " + ProcFile( nVal + 1 )

///////////////////////////////////////////////////////////////////
// Затенение на форме / Darken the form
FUNCTION OverlayCreate( hWnd, aClr1, aClr2, aClr3, nTrans,;
                        cMsg, cFont, nFSize, aFontClr1, aFontClr2, aFontClr3 )
   LOCAL hForm, cFocu, nForm, cForm, cForm2
   LOCAL nY, nX, nH, nW, aBClr, aFClr, lRet
   LOCAL ow := _WindowObj( hWnd )  

   aBClr := { aClr1, aClr2, aClr3 }              // сделано так для совместимости с пред.верcией
   aFClr := { aFontClr1, aFontClr2, aFontClr3 }  // done this way for compatibility with the previous version

   IF ( lRet := HB_ISOBJECT(ow) )
      hForm := ow:Handle
      cForm := ow:Name
      nForm := ow:Index
      cFocu := ow:FocusedControl

      cForm2 := cForm + "Shadow"
      nY   := ow:Row    // GetProperty(cForm, "Row"  )
      nX   := ow:Col    // GetProperty(cForm, "Col"  )
      nW   := ow:Width  // GetProperty(cForm, "Width" )
      nH   := ow:Height // GetProperty(cForm, "Height")
                        // ^^^ - это длинные процедуры через GetFormIndex(cForm), т.е. AScan(...)

      DEFINE WINDOW &cForm2 AT nY, nX WIDTH nW HEIGHT nH TITLE " " ;
         MODAL NOCAPTION NOSIZE BACKCOLOR aBClr                  ;
         ON INIT {|| SetLayeredWindowAttributes( This.Handle, 0, (255 * nTrans) / 100, 0x00000002 ) ,;
                     DoEvents()  }

         nY := nX := 20
         nW := This.ClientWidth  - nX * 2
         nH := This.ClientHeight - nY * 2

         @ nY,nX LABEL Label_Del PARENT &cForm2 WIDTH nW HEIGHT nH VALUE cMsg ;
           FONT cFont SIZE nFSize BOLD FONTCOLOR aFClr BACKCOLOR aBClr CENTERALIGN

      END WINDOW
      ACTIVATE WINDOW &cForm2 NOWAIT

      DO EVENTS

   ENDIF

RETURN lRet

///////////////////////////////////////////////////////////////////
// Затенение на форме / Darken the form
FUNCTION OverlayClose(hWnd)
   LOCAL hForm, cFocu, nForm, cForm, cForm2, lRet
   LOCAL ow := _WindowObj( hWnd ) 

   IF ( lRet := HB_ISOBJECT(ow) )

      hForm  := ow:Handle
      cForm  := ow:Name
      nForm  := ow:Index
      cFocu  := ow:FocusedControl
      cForm2 := cForm + "Shadow"

      // удаляем окно
      IF _IsWindowDefined( cForm2 )
         DoMethod(cForm2, 'Release')
      ENDIF

      DO EVENTS

   ENDIF

RETURN lRet
