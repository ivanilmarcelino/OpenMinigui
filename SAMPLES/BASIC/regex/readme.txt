A regular expression (shortened as regex or regexp), sometimes referred to as rational expression, is a sequence of characters that specifies a match pattern in text. Usually such patterns are used by string-searching algorithms for "find" or "find and replace" operations on strings, or for input validation. Regular expression techniques are developed in theoretical computer science and formal language theory.

It is the most efficient way to validate a data entry

https://en.wikipedia.org/wiki/Regular_expression

Code     : Only numbers, minimum 3, maximum 15

Login    : Only letters and numbers minimum 3, maximum 15

Password :The password must have between 8 and 40 characters, at least one digit, at least one lower case letter, at           least one upper case letter and at least

DNI      : Your country's identification number, Only letters and numbers, minimum 10, maximum 15

URL      : https://www.hmgextended.com/

Address  : 4th street and 14th avenue

Email    : name@gmail.com

IPV4     : 250.100.50.10
An IPv4 address has the format x.x.x.x, where x is called an octet and must be a decimal value between 0 and 255. Octets are separated by periods. An IPv4 address must contain three periods and four octets.

Date     : 2024-06-02
Date validation in format: YYYY-MM-DD, this regular expression does not allow erroneous dates such as February 30 and 31 or February 29 in a year that was not or will not be a Leap Year:

Example
2024-02-29 Correct date is a leap year  
2023-02-29 Incorrect date is not a leap year

The year must have 4 numbers, the month two and the same as the day, for example if the day is 4, 04 will be entered, as well as the month

***
Regular expression

Code
^[0-9]{3,15}$

Login
^[a-zA-Z0-9_-]{3,15}$

Password
^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$

DNI
^[a-zA-Z0-9_-]{10,15}$

URL
^(https?:\/\/)?([\w\-]+(\.[\w\-]+)+)(:\d+)?(\/[^\s]*)?(\?[^\s]*)?(#[^\s]*)?$

Address
^[A-Za-zñÑáéíóúÁÉÍÓÚ0-9\s]+(Street|street|Avenue|avenue |Av|Carrera|Cr|C|Pasaje|Pje|Boulevard|Blvd)?$

Email
^[^@]+@[^@]+\.[a-zA-Z]{2,}$

IPV4 address
^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$

Date
^(19|20)(((([02468][048])|([13579][26]))-02-29)|(\d{2})-((02-((0[1-9])|1\d|2[0-8]))|((((0[13456789])|1[012]))-((0[1-9])|((1|2)\d)|30))|(((0[13578])|(1[02]))-31)))$

Date validation in format: YYYY-MM-DD, this regular expression does not allow erroneous dates such as February 30 and 31 or February 29 in a year that was not or will not be a Leap Year:

Example
2024-02-29 Correct date is a leap year
2023-02-29 Incorrect date is not a leap year

The year must have 4 numbers, the month two and the same as the day, for example if the day is 4, 04 will be entered, as well as the month


Thank you

Marcos Jarrin
marvijarrin@gmail
badasystem.tech
