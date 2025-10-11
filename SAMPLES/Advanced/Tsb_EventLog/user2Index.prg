/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
*/
#define  _HMG_OUTLOG
#include "minigui.ch"
#include "dbinfo.ch"
#define EXT__DBF   ".dbf"
#define EXT__IND   ".cdx"
#define EXT__MEM   ".fpt"
//////////////////////////////////////////////////////////////////////
FUNCTION MenuIndex(oWnd,ky,cBtn)
   LOCAL nY, nX, cForm, Font1, Font2

   ? ProcNL(), oWnd:ClassName, ky, cBtn
   cForm   := oWnd:Name
   Font1   := GetFontHandle( "ComSanMS" )
   Font2   := GetFontHandle( "MnNormal" )
   // координаты вывода окна / window output coordinates
   nY    := GetProperty(cForm, "Row") + GetTitleHeight()
   nY    += GetProperty(cForm, cBtn, "Row") + GetProperty(cForm, cBtn, "Height")
   nX    := GetProperty(cForm, "Col") + GetBorderWidth()
   nX    += GetProperty(cForm, cBtn, "Col") - 4

   SET MENUSTYLE EXTENDED
   SetMenuBitmapHeight( 32 )

   DEFINE CONTEXT MENU OF &cForm
      MENUITEM "Indexation, option 1"  ACTION {|| User1Index() } ICON "iBaseCnf32"  FONT Font1
      SEPARATOR
      MENUITEM "Indexation, option 2"  ACTION {|| User2Index() } ICON "iBaseCnf32"  FONT Font2
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // ПОКАЗ ВЫПАДАЕЩЕГО МЕНЮ / SHOW DROP-DOWN MENU

   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////
FUNCTION User1Index()
   LOCAL cAls, cMsg, cTtl, aWWt, aFileDbf, cSuc, a, s

   ? ProcNL()
   ?? cAls := ALIAS()
   SELECT User2Log

#ifdef KEY_ENG
   cMsg := "Are you sure you want to index the database ?"
   cTtl := "Query"
   cSuc := "Databases successfully indexed:;;"
   aWWt := {"... Wait for the preparation to complete ...", App.ExeName }
#else
   cMsg := "Вы действительно хотите индексировать БД ?"
   cTtl := "Запрос"
   cSuc := "Базы успешно проиндексированы:;;"
   aWWt := {"... Дождитесь завершения подготовки ...", App.ExeName }
#endif

   IF !AlertYesNo(cMsg, cTtl, .T., "iQuest64", 64, { LGREEN, RED }, .T.)
      RETURN NIL
   ENDIF

   // определено в demo_start.prg / defined in demo_start.prg
   //oac:aFileDbf := { {"User2Log" , "User2Log", .T., oac:bIndex1, oac:cStruct1 } ,;
   //                  {"Operat"   , "Operat"  , .T., oac:bIndex2, oac:aStruct2 }    }
   aFileDbf := App.Cargo:aFileDbf

   WaitWindow( aWWt, .T., 600, 16, NIL, WHITE, App.Cargo:aBClrMain )
   a := Set_DataBase_Index(aFileDbf)
   WaitWindow()

   IF LEN(a) > 0
      s := ""
      hb_ForNext( 1, Len( a ), {|i| s += HB_NtoS(i) + ")  " + a[ i ] + ";" } )
      cMsg := cSuc + s + ";"
      AlertInfo( cMsg, "Success", , 64, {{0,120,215}} )
   ENDIF

   ? ProcNL(), "END! ###", cAls, "|"
   DbSelectArea(cAls)
   ?? ALIAS()

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION Set_DataBase_Index(aFileDbf)
   LOCAL cDbf, cAls, lRet, lErr, aUse := {}, a, cErr, bBlk, cFil, aLng
   LOCAL cInd, cPth := App.Cargo:cPathDbf
   LOCAL lInd, aDbf := aFileDbf

#ifdef KEY_ENG
   aLng := { "Not used EXCLUSIVE !;", ";Other users need to leave the database!;",;
             "Error index" }
#else
   aLng := { "Не смог открыть БД в режиме EXCLUSIVE !;", ";Другим пользователям необходимо покинуть базу данных!;",;
             "Ошибка индексации" }
#endif

   cErr := ""
   FOR EACH a IN aDbf
       cDbf := a[1]
       cAls := a[2]
       bBlk := a[4]
       cFil := cPth + cDbf + ".dbf"
       cInd := cPth + cDbf + ".cdx"
       (cAls)->( dbCloseArea() )
       hb_FileDelete( cInd )
       lErr := .T.
       BEGIN SEQUENCE WITH {|e| break( e ) }
          USE ( cPth + cDbf ) ALIAS ( cAls ) NEW EXCLUSIVE
          IF Used() ; AAdd( aUse, cInd + ";" ) ; lErr := .F.
          ENDIF
       END SEQUENCE
       IF lErr
          cErr += aLng[1] + HB_NtoS(hb_enumindex(a))
          cErr += " - " + cAls + " " + cFil + ";"
       ELSE
          lInd := EVal(bBlk, cInd)   // создание cdx нормально
          (cAls)->( dbCloseArea() )
       ENDIF
       DO EVENTS
       BEGIN SEQUENCE WITH {|e| break( e ) }
          USE ( cPth + cDbf ) ALIAS ( cAls ) NEW SHARED
          IF Used() ; lErr := .F.
          ENDIF
       END SEQUENCE
   NEXT
   lRet := .T.
   IF LEN(cErr) > 0
      cErr += aLng[2]
      cErr += ";" + ProcNL() + ";" + ProcNL(1)
      AlertStop( cErr, aLng[3], , 64, {RED} )
      lRet := .F.
   ENDIF

RETURN aUse

*----------------------------------------------------------------------------*
FUNCTION User2Index()    // option Sergej Kiselev
*----------------------------------------------------------------------------*
   LOCAL oac := App.Cargo
   LOCAL cMsg, cTtl, cSuc, aWWt, lUse, cErr, a, s

#ifdef KEY_ENG
   cMsg := "Are you sure you want to index the database ?"
   cTtl := "Query"
   cSuc := "Databases successfully indexed:;;"
   aWWt := {"... Wait for the preparation to complete ...", App.ExeName }
#else
   cMsg := "Вы действительно хотите индексировать БД ?"
   cTtl := "Запрос"
   cSuc := "Базы успешно проиндексированы:;;"
   aWWt := {"... Дождитесь завершения подготовки ...", App.ExeName }
#endif

   IF !AlertYesNo(cMsg, cTtl, .T., "iQuest64", 64, { LGREEN, RED }, .T.)
      RETURN NIL
   ENDIF
   //
   WaitWindow( aWWt, .T., 600, 16, NIL, BLUE, oac:aDlgBColor )
   //
   ? repl("=", 80)
   hb_WAEval({|| // Reindex base
              Local i, f
              ? ProcNL() ; ? hb_ntos(Select())+".", ( f := dbInfo(DBI_FULLPATH) )
              ? "->", Alias(), RecNo(), OrdCount(), IndexOrd(), dbFilter()
              IF OrdCount() > 0
                 FOR i := 1 TO OrdCount()
                     OrdSetFocus(i)
                     ? str(i, 4)+".", OrdSetFocus(), IndexKey()
                 NEXT
                 dbCloseArea()
                 USE ( f ) ALIAS _REINDEX_ EXCLUSIVE
                 i := Seconds()
                 ? "->REINDEX"
                 SET ORDER TO 0
                 GO TOP
                 REINDEX
                 GO TOP
                 dbCloseArea()
                 ?? "Sek. =", Seconds() - i
              ENDIF
              Return Nil
              })
   ? repl("=", 80)
   //
   //CLOSE ALL
   //
   lUse := my_Use_ByApp( , .T., .T., @cErr)   // delete index and check and open

   ? ProcNL()
   ? "USE =", oac:cPathDbf, oac:aFileDbf_New ; ?v oac:aFileDbf_New ; ? "lUse=", lUse, cErr
   //
   my_Use_ByApp( , 0)                    // Close dbf files
   //
   IF lUse
      lUse := Set_DataBase_Open()        // ReOpen
      IF !lUse
         // ERROR message
      ENDIF
   ELSE
      // ERROR message
      cErr += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop( cErr, "ERROR", , 64, {RED} )
   ENDIF
   WaitWindow()

   IF lUse
      s := ""
      a := oac:aFileDbf_New
      hb_ForNext( 1, Len( a ), {|i| s += HB_NtoS(i) + ")  " + a[ i ][1] + EXT__DBF + ";" } )
      cMsg := cSuc + s + ";"
      AlertInfo( cMsg, "Success", , 64, {{0,120,215}} )
   ENDIF

RETURN NIL

*----------------------------------------------------------------------------*
FUNCTION Sets_User2Index()    // recording data in databases
*----------------------------------------------------------------------------*
   LOCAL oac := App.Cargo

   Default oac:aFileDbf_New := {}

   AAdd( oac:aFileDbf_New, Dbf_User2Log() )  // user2Index.prg
   AAdd( oac:aFileDbf_New, Dbf_Operat()   )  // user2Index.prg

RETURN Nil

*----------------------------------------------------------------------------*
FUNCTION my_Use_ByApp(aDbf, lOpen, lDel, cErr)
*----------------------------------------------------------------------------*
   LOCAL oac := App.Cargo, a
   LOCAL lErr, nErr := 0, nUse := 0
   LOCAL cDbf, lDbf, lInd, cAls, cCdp, cMsg
   LOCAL cPth := oac:cPathDbf
   Default lOpen := .T. // .T. - check and open, .F. - check exists, not open
   Default aDbf  := oac:aFileDbf_New    // defined in demo_util.prg
   Default cErr  := ""

   ? ProcNL()
   IF !IsArray(aDbf)
      cMsg := "ERROR! Undefined oac:aFileDbf_New !;;"
      cMsg += ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
      AlertStop( cMsg, "ERROR", , 64, {RED} )
      cErr += cMsg
      RETURN .F.
   ENDIF
   //
   IF !Empty(lDel)      // delete index file
      FOR EACH a IN aDbf
          cDbf := cPth + a[1]
          IF hb_FileExists( cDbf + EXT__DBF )
             hb_FileDelete( cDbf + EXT__IND )
          ENDIF
      NEXT
   ENDIF
   //
   IF IsNumeric(lOpen)  // Close dbf files
      FOR EACH a IN aDbf
          IF Empty(a[3]) ; LOOP
          ENDIF
          IF !Empty(a[4]) .and. Select(a[4]) > 0
             (a[4])->(dbCloseArea())
              a[4] := NIL
          ELSEIF Select(a[2]) > 0
             (a[2])->(dbCloseArea())
          ENDIF
          a[3] := .F.
      NEXT
      RETURN Nil
   ENDIF
   //
   FOR EACH a IN aDbf
       cDbf := cPth + a[1]
       lDbf := hb_FileExists( cDbf + EXT__DBF )
       lInd := hb_FileExists( cDbf + EXT__IND )
       cAls := a[2]
       cCdp := a[5]
       ? cDbf, lDbf, lInd, IsBlock(a[6])
       IF lDbf .and. !lInd .and. IsBlock(a[6])
          lErr := !Eval( a[6], cDbf, cAls + "_TMP", cCdp )
          ?? "lErr=", lErr
          IF lErr
             nErr++
             IF nErr == 1 ; ?
             ENDIF
             cMsg := "*** ERROR ! Creating an index file: "
             cMsg += a[1] + EXT__DBF + " , " + a[1] + EXT__IND
             cErr += cMsg ; ? cMsg
             LOOP
          ENDIF
       ENDIF
       IF lOpen                                 // mode - open dbf
          SELECT 0
          IF Select(cAls) > 0 ; cAls += "_" + hb_ntos(Select())
          ENDIF
          lErr := .T.
          BEGIN SEQUENCE WITH {|e| break( e ) }
          USE ( cDbf ) ALIAS ( cAls ) SHARED CODEPAGE cCdp
          lErr := .F.
          END SEQUENCE
          DO EVENTS
          a[3] := !lErr .and. Used()
          IF a[3] == .T.        // open dbf
             a[4] := Alias()    // real. alias
             nUse++
             IF OrdCount() > 0 ; OrdSetFocus(1)
             ENDIF
             GO TOP
             DO EVENTS
         ENDIF

       ELSEIF lDbf
          nUse++                                 // dbf exists test

       ENDIF
   NEXT

   IF nErr > 0 ; ?
   ENDIF

RETURN ( Len( oac:aFileDbf_New ) == nUse )

*----------------------------------------------------------------------------*
STATIC FUNCTION Dbf_Operat(lStru)
*----------------------------------------------------------------------------*
   LOCAL cDbf := "Operat", cCPage := hb_SetCodepage() //"RU866"
   LOCAL a, cAlias := upper(cDbf)

   IF !Empty(lStru)                // Structure
      a := {}
      RETURN a
   ENDIF
   // FileName  Alias  Used Als CodePage   Index block
   a := {cDbf , cAlias, .F.,  , cCPage , {|cFil,cAls,cCdp|
         Local cInd, cFor, lRet := .F.
         Local lShared, cRdd := RddSetDefault()
         ? "====++++", ProcNL(), ALIAS()
         ? cFil, cAls, cCdp, cRdd
         BEGIN SEQUENCE WITH {|e| break( e ) }
         USE ( cFil ) ALIAS ( cAls ) NEW EXCLUSIVE CODEPAGE cCdp
         lShared := .F.
         //DbUseArea(.T., cRdd, cFil, cAls, lShared, .F., cCdp) // вариант 2 / Option 2
         ?? "Used()=",Used()
         DO EVENTS
         INDEX ON &("KOPERAT")  TAG KOPERAT
         DO EVENTS
         INDEX ON &("OPERAT")   TAG OPERAT
         DO EVENTS
         cInd := "UPPER(OPERAT)"
         cFor := "KOPERAT > 0 .AND. KGROUP < 90 .AND. !DELETED()"
         //INDEX ON &(cInd) TAG KGROUP FOR &(cFor)
         INDEX ON UPPER(FIELD->OPERAT) TAG KGROUP FOR FIELD->KOPERAT > 0 .AND. FIELD->KGROUP < 90 .AND. !DELETED()
         ? "---3---", cInd, cFor, OrdName(), OrdKeyCount()
         ? Alias()
         GO TOP
         DO WHILE !EOF()
            ? Deleted(), RecNo(), FIELD->KOPERAT, FIELD->OPERAT, FIELD->KGROUP
            SKIP
         ENDDO
         GO TOP
         ?
         DO EVENTS
         //
         USE
         lRet := .T.
         END SEQUENCE
         Return lRet
         } }

RETURN AClone( a )

*----------------------------------------------------------------------------*
STATIC FUNCTION Dbf_User2Log(lStru)
*----------------------------------------------------------------------------*
   LOCAL cDbf := "User2Log", cCPage := "RU1251", a
   LOCAL cAlias := upper(cDbf)

   IF !Empty(lStru)                // Structure
      a := {}
      RETURN a
   ENDIF
   // FileName   Alias Used Als CodePage   Index block
   a := {cDbf , cAlias, .F.,  , cCPage , {|cFil,cAls,cCdp|
         Local lRet := .F.
         ? "====++++", ProcNL(), ALIAS(), cFileNoPath(cFil), cAls, cCdp
         BEGIN SEQUENCE WITH {|e| break( e ) }
         ?? "Used()=",Used()
         USE ( cFil ) ALIAS ( cAls ) NEW EXCLUSIVE CODEPAGE cCdp
         DO EVENTS
         INDEX ON &("IDEVENT")    TAG IDEVENT
         DO EVENTS
         INDEX ON &("NUSER")      TAG NUSER     FOR !Deleted() UNIQUE
         DO EVENTS
         INDEX ON &("DTOS(DEVENT) + TEVENT + STR(IDEVENT)") TAG DATEIDEV  FOR !Deleted()
         DO EVENTS
         INDEX ON &("DEVENT")     TAG DATE_EV    FOR !Deleted()
         DO EVENTS
         INDEX ON &("DTOS(DEVENT) + STR(IDEVENT)")  TAG DATE_UNI   FOR !Deleted() UNIQUE
         DO EVENTS
         USE
         lRet := .T.
         END SEQUENCE
         Return lRet
         }}

RETURN AClone( a )

