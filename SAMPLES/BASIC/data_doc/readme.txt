Migration of the data_doc program from clipper to harbour.

DATA_DOC.EXE is a program I created for documenting your data tables.

Besides creating easy to understand interpretations of your data tables,
it can create instruction scripts for another utility, FLATCONV.EXE

These scripts will allow you to create flat file input and output.

I grew tired of creating, over and over again, the programs used to
convert mainframe data to and from .DBF files. With FLATCONV and these
script files, you can convert flat files simply by using script files to
perform the instructions for the conversion engine. I'll not take the
time to explain these script files here, because they are adequately
documented in the FLATCONV conversion routine that can run them.

The parameters for this function are adequately revealed by running
DATA_DOC.EXE without any parameters.

If you need to have DATA_DOC operate on multiple files, it understands
wildcards and directory information in the first parameter.

Author : Phil Barnett
Last modified: 07/11/1993      1:31
Migrated to harbor Minigui Extended
Date   : 30/06/2024
Update : 01/07/2024
Author : Marcos Jarrin
Website: https://badasystem.tech/
Email  : marvijarrin@gmail.com
