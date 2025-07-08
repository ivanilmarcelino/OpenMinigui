Virtual Grid can view large text file

At first I thought it wouldn't be a complicated task, I wanted to use the hb_F* function to process text files, but found out that they don't quite work properly on large files.
I did a simple test which consisted of going to the desired records, going back and calculating the number of records.
Depending on the order of the actions I got different results.
This can be seen by selecting the "Test hb_F* functions" option from the "Test" menu.

So I decided to build my own class. There were performance issues, especially with large files (eg, 70,000,000 records and a size of more than 7 GB) and the jump between hundreds of thousands of records (this is the case when using the vertical slider).
So I thought it might be worth indexing the file you were browsing, but the index file was again taking up a lot of disk space.
I came up with the idea to do something like a record map in my class, which will not be a file but a matrix.
In order to reduce the size of the matrix again, I used 64-bit words to write the values of record number and file pointer (don't worry it works in the 32-bit also), which I was shrinking again.

The record map is created on the fly when navigating the file using the GoTo and Skip methods, and is completely created when the CountLines method is called.

The class is probably not without errors, but it may be helpful for you.

I used the Virtual Grid control to browse the files. To be able to read the entire file into the Grid, you need to know the number of records. The counting process may take some time with large files.
So, I did three browsing variants:
First: only the beginning of the file is loaded. The moment cell navigation is moved near the end of the list, the next part of the file is loaded - incremental loading.
Second: records are counted before browsing.
Third: counting the number of records is performed in a separate thread and is constantly updated while browsing the Grid.

Edward