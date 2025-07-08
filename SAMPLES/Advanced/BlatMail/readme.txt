Configuring BLAT with STUNNEL

- Configure stunnel:

Unfortunately, blat does not support SSL connections. However another free utility, stunnel (secure tunnel?), can convert data from an SSL-ignorant program like blat and send it to an SSL-aware computer or connection. Stunnel can do lots of other things too.  Installation turned out to be quite simple, basically the same on Windows XP, Windows Vista, Windows 7, and Windows 2008 R2.

•Download stunnel-4.xx-installer.exe from stunnel.org.
•Run it and accept the defaults. This installs the software on the disk, in c:\Program Files\stunnel on a 32-bit system or c:\Program Files (x86)\stunnel on a 64-bit system.
•In the "X:\Program Files (x86)\stunnel" folder save a copy of stunnel.conf  to  stunnel.conf.orig.txt
•In Start/Programs go to the new stunnel folder and right-click on "Edit Stunnel.conf." Click Run as Administrator (Vista or W7), or click Open (XP). This brings the configuration file into Notepad with rights to modify it. You may be prompted for permission in this and following steps.
•Delete everything in the file, and copy in the four lines example below, modifying them for your use. "Accept" is the port on which blat sends the email (this can be changed), and "connect" is the server name and port that your ISP wants you to use.

client = yes
[ssmtp]
accept  = 25
connect = mail.btinternet.com:465

•Save and quit.
•In Start/Programs, right-click "Service Install" and click Run as Administrator.
•In Start/Programs, right-click "Service Start"   and click Run as Administrator.

In the script which runs blat, or in the registry, you will want to specity the server as follows: "-server localhost:25"



- Configure blat:

Download "blat" (probably v3.1.1) and copy blat.exe, blat.lib, and blat.dll to your "utils" folder.

Syntax:
blat -install[SMTP|NNTP|POP3|IMAP] <server addr> <sender email addr> [<try n times> [<port> [<profile> [<username> [<password>]]]]]

Old config - i.e. not using stunnel for SSL:
     blat -profile
     REM                smtp-server          sender-email         tries  port  profile-name  login-account        password
     blat -installSMTP  mail.btinternet.com  blah@btinternet.com  1      25    myprofile     blah@btinternet.com  blahblahblah
     blat -profile

New config - i.e. using stunnel for SSL:
     blat -profile
     blat -profile -delete myprofile
     blat -profile
     blat -installSMTP localhost blah@btinternet.com 1 25 myprofile blah@btinternet.com blahblahblah
     blat -profile

Test sending via stunnel via your BT account to your Apple account with:
     (echo test-body)      >test-body.txt
     (echo test-attachment)>test-attachment.txt
     blat -profile
     blat test-body.txt -s "test from %computername%" -t blah@me.com -p myprofile -attach test-attachment.txt -debug