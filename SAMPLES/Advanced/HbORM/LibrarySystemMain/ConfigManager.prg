/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : ConfigManager Class
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrín
 Email         : marvijarrin@gmail.com
 Date          : 15/07/2025
 Update        : 03/08/2025
 Rev           : 0.1

*/

#include "hbclass.ch"
#include "minigui.ch"

// ----------------------------------------------------------------------------
// ConfigManager Class
// ----------------------------------------------------------------------------
CLASS ConfigManager
    DATA cIniFile  AS STRING
    DATA hConfig   AS HASH

    METHOD New(cFile) CONSTRUCTOR
    METHOD WriteIni(hConfig)
    METHOD WriteIniEntry(cKey,xValue) PROTECTED
    METHOD loadConfig()
    METHOD getConfig(cKey, xDefault)
ENDCLASS

METHOD New(cFile) CLASS ConfigManager
    ::cIniFile := cFile
    ::hConfig  := {=>}
RETURN Self

METHOD WriteIni(hConfig) CLASS ConfigManager

    LOCAL lSuccess := .T.

    IF !File(::cIniFile)

       BEGIN INI FILE ::cIniFile
            HEval( hConfig, {|cKey,xValue| ::WriteIniEntry(cKey,xValue)  } )
       END INI

    ENDIF

RETURN lSuccess

METHOD PROCEDURE WriteIniEntry(cKey,xValue) CLASS ConfigManager
      SET SECTION 'Main' ENTRY cKey TO xValue
RETURN

/**
 * Loads configuration from INI file
 * return void
 */
METHOD loadConfig() CLASS ConfigManager

   LOCAL xValue01
   LOCAL xValue02

   BEGIN INI FILE (::cIniFile)
      GET xValue01 SECTION 'Main' ENTRY 'LOAN_DAYS' DEFAULT 14
      GET xValue02 SECTION 'Main' ENTRY 'LATE_FEE'  DEFAULT  1
   END INI

   ::hConfig := hash( 'LOAN_DAYS', xValue01, 'LATE_FEE', xValue02 )

RETURN ::hConfig

RETURN NIL

/**
 * Gets configuration value
 * param cKey String Configuration key
 * param xDefault Any Default value
 * return Any Configuration value
 */
METHOD getConfig(cKey, xDefault) CLASS ConfigManager
    IF empty(::hConfig)
        ::loadConfig()
    ENDIF
RETURN HGet(::hConfig, cKey)
