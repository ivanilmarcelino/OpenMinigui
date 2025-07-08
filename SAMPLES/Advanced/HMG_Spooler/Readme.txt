When you have a local printer that is a 32-bit Win Printer
and you can't find the driver corresponding to 64 bits for the new PC
connected to the net, what can you do?
Put this Spooler and molds on the Windows PDF printer
With destination the folder managed by the Spooler which of course
It is shared on the net and voila the 32-bit printer becomes local
at the net and reusable even at 64-bit.

*******************************************************************************
This utility requires a folder called Sumatrapdf and containing
the Sumatrapdf.exe file
*******************************************************************************

Ini file management instructions

[WorkDir]
Folder=  Folder on which to send the files to print E.G.       Q:\CLIENTS\Spooler\
Type=    Separate managed gestured file extensions with comma  Default Pdf,Txt
Delay=   Pause set between one printing and the other          Default 50
Msg=     View all posts                                        Default T
Printer= Printer name to use                                   Default the predefined one
Clean=   Delete the print queue once printed                   Default T
Auto=    Manages the print queues at startup                   Default F
Dialog=  Requires the printer ownership window                 Default F

The "Restart Print Queues" Command Option is also useful for tests without initializing
the continuous control of the Spooler folder.

Copyright 2025 Pierpaolo Martinello  assistenza [at] pcmodula.it

This program is a free software; You can redistribute it and/or change it below
The terms of the license of the GNU general public published by the free software
Foundation; o Version 2 of the license or (at your option) later
version.

This program is distributed in the hope that it will be useful, but without anyone
WARRANTY; without even the implicit guarantee of marketability or suitability for a
Particular purpose. See the License of the Gnu General Public for more details.

You should have received a copy of the GNU general public license together with
This software; See the copy of the file. Otherwise, write on the free software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, but 02111-1307 USA (O
Visit the website http://www.gnu.org/).

As a special exception, you have the authorization for further uses of the text
Content in this version of minisql.

The exception is that, if you connect this source with other files
To produce an executable, this does not in itself cause the resulting executable
be covered by the GNU general public license.

The use of this executable is in no limited way