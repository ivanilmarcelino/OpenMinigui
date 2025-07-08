You can check the properties of the computer that runs the application and 
compare them with some algorithm that calculates the checksum of these 
properties.

In the example below, processors, disks, network cards and UUID are checked. 
If the calculated checksum of these properties does not match the sum saved 
in the example.ini file (it can of course be any database, etc.), the program 
will not run.

Activation methods can be implemented through various mechanisms, here I used 
a simple mechanism of calling the application with the "/ActivationByLicensor" 
parameter.

Edward