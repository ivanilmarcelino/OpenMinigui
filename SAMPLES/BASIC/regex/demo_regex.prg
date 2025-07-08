 /*

 BadaSystem
 Program       : demo_regex
 Modulo        : demo_regex.prg
 Compilador    : MINIGUI - Harbour Win32 GUI
 Link          : BCC 32 bit
 Autor         : Marcos Jarrín
 email         : marvijarrin@gmail.com
 website       : badasystem.tech
 Date          : 21/06/2024
 Update        : 25/06/2024
 Rev           : 1.0

*/

#include "minigui.ch"

STATIC aState := { .F., .F., .F., .F., .F., .F., .F., .F., .F. }

/*

Entering test data

*/
PROCEDURE Main()

   LOCAL nRow, nCol

   nRow := 20
   nCol := 10

   SET TOOLTIP MAXWIDTH TO 256

   DEFINE WINDOW RegexInput ;
      AT 0, 0 ;
      WIDTH 550 ;
      HEIGHT 400 ;
      TITLE "Entering test data" ;
      MAIN ;
      FONT 'ARIAL' SIZE 9 ;
      NOMAXIMIZE

      @ nROW, nCol LABEL CODE_LABEL VALUE 'Code :' WIDTH 80
      @ nROW, nCol + 70 TEXTBOX code MAXLENGTH 15 ON ENTER ValidaCode() TOOLTIP "Numbers only, minimum 3, maximum 15" PLACEHOLDER "123456"
      @ nROW, nCol + 400 IMAGE simbol1 PICTURE "" TRANSPARENT

      @ nROW + 30, nCol LABEL LOGIN_LABEL VALUE 'Login :' WIDTH 80
      @ nROW + 30, nCol + 70 TEXTBOX login WIDTH 180 MAXLENGTH 25 ON ENTER ValidateLogin() TOOLTIP "Only letters and numbers minimum 3, maximum 15" PLACEHOLDER "Alexander"
      @ nROW + 30, nCol + 400 IMAGE simbol2 PICTURE "" TRANSPARENT

      @ nROW + 60, nCol LABEL PASSWORD_LABEL VALUE 'Password :' WIDTH 80
      @ nROW + 60, nCol + 70 TEXTBOX password WIDTH 250 MAXLENGTH 30 ON ENTER ValidPassword() TOOLTIP "Password, between 8 and 40 characters, at least 1 digit, at least 1 lower case letter, at least 1 upper case letter and at least one alphanumeric character"
      @ nROW + 60, nCol + 400 IMAGE simbol3 PICTURE "" TRANSPARENT

      @ nROW + 90, nCol LABEL DNI_LABEL VALUE 'DNI :' WIDTH 80
      @ nROW + 90, nCol + 70 TEXTBOX dni WIDTH 120 MAXLENGTH 15 ON ENTER ValidDNI() TOOLTIP "Only letters and numbers, minimum 10, maximum 15" PLACEHOLDER "1317896541"
      @ nROW + 90, nCol + 400 IMAGE simbol4 PICTURE "" TRANSPARENT

      @ nROW + 120, nCol LABEL POSTION_LABEL VALUE 'URL :' WIDTH 80
      @ nROW + 120, nCol + 70 TEXTBOX url WIDTH 200 MAXLENGTH 30 ON ENTER ValidateURL() TOOLTIP "url, https://www.hmgextended.com/" PLACEHOLDER "https://www.hmgextended.com/"
      @ nROW + 120, nCol + 400 IMAGE simbol5 PICTURE "" TRANSPARENT

      @ nROW + 150, nCol LABEL ADDRESS_LABEL VALUE 'Address :' WIDTH 80
      @ nROW + 150, nCol + 70 TEXTBOX ADDRESS WIDTH 300 MAXLENGTH 50 ON ENTER ValidateAddress() TOOLTIP "You need to have the word Street|street|Avenue|avenue" PLACEHOLDER "4th street and 14th avenue"
      @ nROW + 150, nCol + 400 IMAGE simbol6 PICTURE "" TRANSPARENT

      @ nROW + 180, nCol LABEL EMAIL_LABEL VALUE 'Email :' WIDTH 80
      @ nROW + 180, nCol + 70 TEXTBOX email WIDTH 200 MAXLENGTH 25 ON ENTER ValidateEmail() TOOLTIP "Email" PLACEHOLDER "acme@gmail.com"
      @ nROW + 180, nCol + 400 IMAGE simbol7 PICTURE "" TRANSPARENT

      @ nROW + 210, nCol LABEL IPV4_LABEL VALUE 'IPV4 :' WIDTH 80
      @ nROW + 210, nCol + 70 TEXTBOX ipv4 WIDTH 110 MAXLENGTH 15 ON ENTER ValidateIPV4() TOOLTIP "IPV4" PLACEHOLDER "250.100.50.10"
      @ nROW + 210, nCol + 400 IMAGE simbol8 PICTURE "" TRANSPARENT

      @ nROW + 240, nCol LABEL DATE_LABEL VALUE 'Date :' WIDTH 100
      @ nROW + 240, nCol + 70 TEXTBOX date_1 WIDTH 100 MAXLENGTH 10 ON ENTER ValidateDate() TOOLTIP "Enter date in yyyy-mm-dd format" PLACEHOLDER "2024-06-02"
      @ nROW + 240, nCol + 400 IMAGE simbol9 PICTURE "" TRANSPARENT

      @ nROW + 280, nCol + 200 BUTTON ACCEPT CAPTION 'Accept ' ACTION ValidAll()

   END WINDOW

   GenPass( 20 )
   RegexInput.code.SetFocus

   CENTER WINDOW RegexInput
   ACTIVATE WINDOW RegexInput

RETURN


/*

Generate random key

*/
PROCEDURE GenPass( nSize )

   LOCAL cSet1 := 'QWERTYUIOPASDFGHJKLZXCVBNM'
   LOCAL cSet2 := 'qwertyuiopasdfghjklzxcvbnm'
   LOCAL cSet3 := '@$!%?&'
   LOCAL cSet4 := '1234567890'
   LOCAL nLen1, nLen2, nLen3, nLen4
   LOCAL cLet1, cLet2, cLet3, cLet4
   LOCAL cPass := ""
   LOCAL nX

   nLen1 := Len( cSet1 )
   nLen2 := Len( cSet2 )
   nLen3 := Len( cSet3 )
   nLen4 := Len( cSet4 )

   nSize := Int( nSize / 4 )

   FOR nX := 1 TO nSize

      cLet1 := SubStr( cSet1, Random( nLen1 ), 1 )
      cLet2 := SubStr( cSet2, Random( nLen2 ), 1 )
      cLet3 := SubStr( cSet3, Random( nLen3 ), 1 )
      cLet4 := SubStr( cSet4, Random( nLen4 ), 1 )

      cPass += cLet1 + cLet2 + cLet3 + cLet4

   NEXT

   RegexInput.password.value := cPass

RETURN


/*

Validate Code

*/
FUNCTION ValidaCode()

   LOCAL cRegEx := '^[0-9]{3,15}$'
   LOCAL cCode := ""

   cCode := AllTrim( RegexInput.code.value )
   if ! hb_regexMatch( cRegEx, cCode )
      RegexInput.simbol1.picture := "cancel.png"
      msginfo( "Incorrect Code, Only numbers, minimum 3, maximum 15" )
      RegexInput.code.SetFocus
      RegexInput.simbol1.picture := "cancel.png"
      aState[ 1 ] := .F.
   ELSE
      RegexInput.simbol1.picture := "ok.png"
      aState[ 1 ] := .T.
      RegexInput.login.SetFocus
   ENDIF

RETURN .T.



/*

Validate login

*/
FUNCTION ValidateLogin()

   LOCAL cRegEx := '^[a-zA-Z0-9_-]{3,15}$'
   LOCAL cName := ""

   cName := AllTrim( RegexInput.login.value )
   if ! hb_regexMatch( cRegEx, cName )
      RegexInput.simbol2.picture := "cancel.png"
      msginfo( "Incorrect Login, Only letters and numbersminimum 3, maximum 15" )
      RegexInput.login.SetFocus
      aState[ 2 ] := .F.
   ELSE
      RegexInput.simbol2.picture := "ok.png"
      aState[ 2 ] := .T.
      RegexInput.password.SetFocus
   ENDIF

RETURN .T.

/*

Validate password

*/
FUNCTION ValidPassword()

   LOCAL cRegEx := "^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$Ã!%*?&])[A-Za-z\d@$Ã!%*?&]{8,}$"
   LOCAL cPassword := ""

   // Digits, lower and upper case letters and symbols
   // The password must have between 8 and 40 characters, at least one digit,
   // at least one lower case letter, at least one upper case letter and at least
   // one alphanumeric character.

   cPassword := AllTrim( RegexInput.password.value )

   if ! hb_regexMatch( cRegEx, cPassword )
      RegexInput.simbol3.picture := "cancel.png"
      msginfo( "Incorrect Password,The password must have between 8 and 40 characters, at least one digit, at least one lower case letter, at least one uppercase letter and at least one special character @$!%?&" )
      RegexInput.password.SetFocus
      aState[ 3 ] := .F.
   ELSE
      RegexInput.simbol3.picture := "ok.png"
      aState[ 3 ] := .T.
      RegexInput.dni.SetFocus
   ENDIF

RETURN .T.


/*

Validate DNI

*/
FUNCTION ValidDNI()

   LOCAL cRegEx := '^[a-zA-Z0-9_-]{10,15}$'
   LOCAL cDNI := ""

   cDNI := AllTrim( RegexInput.dni.value )
   if ! hb_regexMatch( cRegEx, cDNI )
      RegexInput.simbol4.picture := "cancel.png"
      msginfo( "Incorrect DNI,Only letters and numbers, minimum 10, maximum 15" )
      RegexInput.dni.SetFocus
      aState[ 4 ] := .F.
   ELSE
      RegexInput.simbol4.picture := "ok.png"
      aState[ 4 ] := .T.
      RegexInput.url.SetFocus
   ENDIF

RETURN .T.

/*

Validate URL

*/
FUNCTION ValidateURL()

   LOCAL cRegEx := '^(https?:\/\/)?([\w\-]+(\.[\w\-]+)+)(:\d+)?(\/[^\s]*)?(\?[^\s]*)?(#[^\s]*)?$'
   LOCAL cURL := ""

   cURL := AllTrim( RegexInput.url.value )
   if ! hb_regexMatch( cRegEx, cURL )
      RegexInput.simbol5.picture := "cancel.png"
      msginfo( "Incorrect URL" )
      RegexInput.url.SetFocus
      aState[ 5 ] := .F.
   ELSE
      RegexInput.simbol5.picture := "ok.png"
      aState[ 5 ] := .T.
      RegexInput.address.SetFocus
   ENDIF

RETURN .T.


/*

Validate Address

*/
FUNCTION ValidateAddress()

   LOCAL cRegEx := '^[A-Za-zñÑáéíóúÁÉÍÓÚ0-9\s]+(Street|street|Avenue|avenue |Av|Carrera|Cr|C|Pasaje|Pje|Boulevard|Blvd)?$'
   LOCAL cAddress := ""

   cAddress := AllTrim( RegexInput.address.value )
   if ! hb_regexMatch( cRegEx, cAddress )
      RegexInput.simbol6.picture := "cancel.png"
      msginfo( "Incorrect Address, You need to have the word Street|street|Avenue|avenue" )
      RegexInput.address.SetFocus
      aState[ 6 ] := .F.
   ELSE
      RegexInput.simbol6.picture := "ok.png"
      aState[ 6 ] := .T.
      RegexInput.email.SetFocus
   ENDIF

RETURN .T.

/*

Validate Email

*/
FUNCTION ValidateEmail()

   LOCAL cRegEx := '^[^@]+@[^@]+\.[a-zA-Z]{2,}$'
   LOCAL cEmail := ""

   cEmail := AllTrim( RegexInput.email.value )
   if ! hb_regexMatch( cRegEx, cEmail )
      RegexInput.simbol7.picture := "cancel.png"
      msginfo( "Incorrect Email" )
      RegexInput.email.SetFocus
      aState[ 7 ] := .F.
   ELSE
      RegexInput.simbol7.picture := "ok.png"
      aState[ 7 ] := .T.
      RegexInput.ipv4.SetFocus
   ENDIF

RETURN .T.

/*

Validate an IPV4 address

*/
FUNCTION ValidateIPV4()

   LOCAL cRegEx := '^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$'
   LOCAL cIPV4 := ""


   cIPV4 := AllTrim( RegexInput.ipv4.value )
   if ! hb_regexMatch( cRegEx, cIPV4 )
      RegexInput.simbol8.picture := "cancel.png"
      msginfo( "Incorrect IPV4. Each number between the dots must be in the range 0 to 255" )
      RegexInput.ipv4.SetFocus
      aState[ 8 ] := .F.
   ELSE
      RegexInput.simbol8.picture := "ok.png"
      aState[ 8 ] := .T.
      RegexInput.date_1.SetFocus
   ENDIF

RETURN .T.

/*

Validate a date

*/
FUNCTION ValidateDate()
   // yyyy-mm-dd
   LOCAL cRegEx := '^(19|20)(((([02468][048])|([13579][26]))-02-29)|(\d{2})-((02-((0[1-9])|1\d|2[0-8]))|((((0[13456789])|1[012]))-((0[1-9])|((1|2)\d)|30))|(((0[13578])|(1[02]))-31)))$'
   LOCAL cDate := ""


   cDate := AllTrim( RegexInput.date_1.value )
   if ! hb_regexMatch( cRegEx, cDate )
      RegexInput.simbol9.picture := "cancel.png"
      msginfo( "Incorrect date, yyyy-mm-dd" )
      RegexInput.date_1.SetFocus
      aState[ 9 ] := .F.
   ELSE
      RegexInput.simbol9.picture := "ok.png"
      aState[ 9 ] := .T.
      RegexInput.accept.SetFocus
   ENDIF

RETURN .T.


/*

Validate fields

*/
PROCEDURE ValidAll()

   // code
   IF aState[ 1 ] == .F.
      IF Empty( RegexInput.code.value )
         RegexInput.simbol1.picture := "cancel.png"
         msgbox( "Code field cannot be empty" )
      ELSE
         RegexInput.simbol1.picture := "cancel.png"
         msginfo( "Incorrect Code, Only numbers, minimum 3, maximum 15" )
         RegexInput.code.fontcolor := { 200, 100, 100 }
      ENDIF
   ENDIF

   // login
   IF aState[ 2 ] == .F.
      IF Empty( RegexInput.login.value )
         RegexInput.simbol2.picture := "cancel.png"
         msgbox( "Login field cannot be empty" )
      ELSE
         RegexInput.simbol2.picture := "cancel.png"
         msginfo( "Incorrect Login, Only letters and numbersminimum 3, maximum 15" )
      ENDIF
   ENDIF

   // password
   IF aState[ 3 ] == .F.
      IF Empty( RegexInput.password.value )
         RegexInput.simbol3.picture := "cancel.png"
         msgbox( "Password field cannot be empty" )
      ELSE
         RegexInput.simbol3.picture := "cancel.png"
         msginfo( "Incorrect Password,The password must have between 8 and 40 characters, at least one digit, at least one lower case letter, at least one uppercase letter and at least one special character @$!%?&" )
      ENDIF
   ENDIF

   // DNI
   IF aState[ 4 ] == .F.
      IF Empty( RegexInput.dni.value )
         RegexInput.simbol4.picture := "cancel.png"
         msgbox( "DNI field cannot be empty" )
      ELSE
         RegexInput.simbol4.picture := "cancel.png"
         msginfo( "Incorrect DNI,Only letters and numbers, minimum 10, maximum 15" )
      ENDIF
   ENDIF

   // URL
   IF aState[ 5 ] == .F.

      IF Empty( RegexInput.url.value )
         RegexInput.simbol5.picture := "cancel.png"
         msgbox( "URL field cannot be empty" )
      ELSE
         RegexInput.simbol5.picture := "cancel.png"
         msginfo( "Incorrect URL" )
      ENDIF

   ENDIF

   // Address
   IF aState[ 6 ] == .F.

      IF Empty( RegexInput.address.value )
         RegexInput.simbol6.picture := "cancel.png"
         msgbox( "Address field cannot be empty" )
      ELSE
         RegexInput.simbol6.picture := "cancel.png"
         msginfo( "Incorrect Address, You need to have the word Street|street|Avenue|avenue" )
      ENDIF

   ENDIF

   // email
   IF aState[ 7 ] == .F.

      IF Empty( RegexInput.email.value )
         RegexInput.simbol7.picture := "cancel.png"
         msgbox( "Email field cannot be empty" )
      ELSE
         RegexInput.simbol7.picture := "cancel.png"
         msginfo( "Incorrect Email" )
      ENDIF

   ENDIF

   // IPV4
   IF aState[ 8 ] == .F.

      IF Empty( RegexInput.ipv4.value )
         RegexInput.simbol8.picture := "cancel.png"
         msgbox( "IPV4 field cannot be empty" )
      ELSE
         RegexInput.simbol8.picture := "cancel.png"
         msginfo( "Incorrect IPV4. Each number between the dots must be in the range 0 to 255" )
      ENDIF

   ENDIF

   // Date
   IF aState[ 9 ] == .F.

      IF Empty( RegexInput.date_1.value )
         RegexInput.simbol9.picture := "cancel.png"
         msgbox( "Date field cannot be empty" )
      ELSE
         RegexInput.simbol9.picture := "cancel.png"
         msginfo( "Incorrect date, yyyy-mm-dd. The year must have 4 numbers, the month two and the same as the day, for example if the day is 4, 04 will be entered, as well as the month" )
      ENDIF

   ENDIF

   //
   IF aState[ 1 ] == .T. .AND. aState[ 2 ] == .T. .AND. aState[ 3 ] == .T. .AND. aState[ 4 ] == .T. .AND. aState[ 5 ] == .T. .AND. aState[ 6 ] == .T. .AND. aState[ 7 ] == .T. .AND. aState[ 8 ] == .T. .AND. aState[ 9 ] == .T.
      RegexInput.simbol1.picture := "ok.png"
      RegexInput.simbol2.picture := "ok.png"
      RegexInput.simbol3.picture := "ok.png"
      RegexInput.simbol4.picture := "ok.png"
      RegexInput.simbol5.picture := "ok.png"
      RegexInput.simbol6.picture := "ok.png"
      RegexInput.simbol7.picture := "ok.png"
      RegexInput.simbol8.picture := "ok.png"
      RegexInput.simbol9.picture := "ok.png"
      msgbox( "Correct test" )
   ENDIF

RETURN
