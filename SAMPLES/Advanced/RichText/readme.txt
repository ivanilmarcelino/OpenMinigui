Notes on RichText() Class for Clipper/FiveWin
10/25/97
Version 1.00

Author: 	Tom Marchione
Internet:	tmarchione@compuserve.com

LEGAL STUFF

All source code is Copyright (C) 1997 Thomas R. Marchione.  As a
Clipper/Fivewin user, you are granted the right to use and modify the code for
application work, provided that all file headers are kept intact, you do not
offer the source code for sale, and do not port the code to another language.
Future versions of this class may or may not be offered for public consumption
by the author.  Use the code at your own risk -- no warranties are expressed or
implied.  Etc., etc...


GETTING STARTED

If you're really curious about what this thing does, compile and run the sample
application (just look at the "BUILDRTF.BAT" batch file -- it's really simple,
if you're already a Clipper/Fivewin developer).  Then open the output file
(RTFDEMO.RTF) in Microsoft Word (personally, I haven't tested it with anything
else). If you don't have time to to compile and run the app, I've included an
unedited copy of the output file in the uploaded ZIP file.


OVERVIEW

RichText() is a class written for Clipper/Fivewin, designed to 
export formatted data directly to Rich Text Format (RTF) files 
from within an application.  RichText() allows you to leverage the 
power of common word processors as report formatting engines.

Version 1.0 of the class includes: (1) the source code, build and 
README files, (2) Clipper directives for xBASE-style command 
implementation, and (3) a small sample application.  Version 1.0 
of the class is being offered at no charge.  This version 
implements a small subset of the RTF specification, as taken from 
Microsoft documentation.    RTF is a very detailed specification, 
and the intent of Version 1.0 is to provide easy access to the 
most basic and useful features of RTF, including attributes such 
as page setup, fonts, paragraph formatting, character formatting, 
headers & footers, tables, etc.  To date, most of my testing has 
been done with Microsoft Word.


WHAT IS RTF?

RTF is an open Microsoft file specification for transferring 
formatted text between applications.  As such, it it's a great way 
to move formatted database information into word processors, in a 
true word processing format.
 
HOW CAN RTF HELP ME?

RTF is a great solution in at least three situations:

1. When there is a need to manipulate output in word processors, 
or integrate database information into other formatted 
documents.

2. When there is a requirement to build a custom report that 
must meet strict, detailed formatting guidelines.

3. When there is a need to publish a "turnkey" report that might 
include different formats, page orientations, fonts, etc. in 
a single document (my personal favorite…).

The RichText() class is a simple, low-overhead solution for any of 
these situations.  You can produce one-of-a kind, free-form word-
processing documents (for a good example, see the sample provided 
with the class).  The class is inherently oriented toward 
paragraphs, bullets, hanging indents, page orientation, etc., not 
standard database reporting fare such as column headers, 
subtotals, etc. (although you can easily dump data into nicely-
formatted tables).  Moreover, the output files are true word-
processing files, without extraneous spaces, carriage returns, and 
other "lint" that makes life difficult in a word processing 
context.  If you have ever tried to pass off a flat text output 
file (or even some RTF files produced by third party report 
generators) as "word processor compatible", you'll know what I 
mean.  Compatible? Yes.  Usable? Hardly.


ESSENTIAL FEATURES OF RTF

For detailed information about RTF, consult the Microsoft specification,
mentioned earlier in this document.  It is also helpful to analyze the contents
of RTF files generated from word processors.  Although the xBASE layer of
RichText() makes many of the technical details transparent, it will be helpful
to clarify a few issues here.

RTF uses 7-bit ASCII characters (0-127) to represent formatting codes and data
(higher-order ASCII characters must be represented in hexadecimal, which is
handled transparently by RichText()).  The two essential formatting characters
in RTF are "\" (backslash) and "{}" (curly braces).  All formatting commands are
preceded by a backslash, and all "groups" are enclosed in curly braces.  Groups
delineate portions of a document to which combinations of formatting information
may apply.  Groups can be nested; the RTF file itself is considered a group, so
all RTF files have at least one pair of curly braces, which encloses the entire
contents of the file.

All RTF files have an RTF header (which is to be distinguished from the headers
and footers that you may place in an RTF document).  The RTF header appears at
the top of the file and contains information about fonts, colors, styles, etc.
For example, the font table within the RTF header contains a list of all of the
fonts that are used in the document.  To switch fonts later the document, an
index into this font table must be used.  A similar system is used for colors.
RichText() takes care of the RTF header for you.

An application that reads RTF files is called an RTF Viewer.  For the most part,
data items that you write to an RTF file appear within RTF Viewers in the
sequence that they were written.  However certain RTF commands implement
alternate "destinations", such as headers and footers.  RTF also supports the
"sections" that are found in Microsoft Word, with section-specific layouts and
formats.


WHERE TO GO FROM HERE

If you like the capabilities that are demonstrated in the sample, take a look at
the source code for the sample (RTFDEMO.PRG).  From there, look at the header
file (RICHTEXT.CH), and then dive into the class itself (RICHTEXT.PRG).  It's
more tedious than complicated -- if you are willing to learn a few key RTF
concepts, you'll find that most of the code is actually very simple and basic.

A word of warning -- it's very easy to break an RTF file.  Surprisingly enough,
MS-Word can throw up and die if you misplace even one seemingly innocent code.
So, if you decide to change the base class, make sure that you test each change!

So far, I have made no effort to explore the real possibilities of the class, in
terms of integrating it into more traditional reporting systems, or building
better application-level components to meet recurring, standard requirements.
If you have any ideas or suggestions, I'd love to hear them.

Have fun!

Tom
