:userdoc.
:title.Technical supplement to the Weasel manual
:docprof toc=12.

.***********************************
.*   INTRODUCTION
.***********************************

:h1.Technical supplement to the Weasel manual

:artwork name='weasel.bmp' align=center.

:p.
This manual contains additional information about Weasel,
primarily for the use of people who want to transfer data
between Weasel and another program.

:p.Non-technical users can afford to ignore this supplement.

.***********************************
.*   HOW MAIL IS STORED
.***********************************

:h1.How mail is stored
:hp2.How mail is stored:ehp2.

:p.When you run the Setup program that comes with Weasel, you
specify the name of a disk directory called the "mail root"
directory. For each user that has a POP3 account, a subdirectory
is created in this directory to hold the mail for that user. The
username is used as the name of the subdirectory.
In addition, one or two special subdirectories are created&colon.

:dl break=all.
:dt.    postmaster
:dd.The SMTP standard specifies that a mail server must always be
able to receive mail for a 'postmaster' account. Note, however, that
you can satisfy this requirement by creating an alias that will
accept the 'postmaster' mail. If you have an alias that matches
'postmaster' - e.g. an alias called 'postmaster' or 'post*' or
even simply '*' - then Setup will not create a 'postmaster' directory.
If you have such an alias, but you still want to have a specific
'postmaster' account, you will have to create the account manually.
:dt.    forward
:dd.This subdirectory is used to hold outbound mail while it is
waiting to be sent out. Files in the 'forward' subdirectory have
a special prefix string, as described in
:link reftype=hd refid=forward.a later section:elink..
:edl.

:p.In an earlier version of Weasel, Setup would create a mail directory
for a special username 'unknown' if you selected the option "Accept
mail for unknown users". That option no longer exists. If you want
to accept mail for unknown users, you can do so by creating an alias
with the name '*', and making the expansion of that alias include
a suitable username (for example, a user called 'unknown') that will
accept mail for usernames that are otherwise unmatched. In that case,
it is your responsibility to create a suitable account to receive
that mail.

:p.Each e-mail message is stored as a plain text file, in the standard
e-mail format: the header lines, at least one blank line, and then
the body of the message. (Nothing special has to be done about
attachments, because these are simply part of the message body.)
The file names have the form ????????.MSG, where the first eight
characters have no significance except that they are chosen in such
a way as not to conflict with the name of any other file in the
directory. The message body is literally as it was supplied by
the sender, except for some
:link reftype=hd refid=stuffing.byte-stuffing:elink. that is
required by the e-mail protocols.

:p.At various times files with names ending in special extensions
such as ### are created in the mail directories. These are temporary
files, and in most cases they will be deleted before you see them.

:p.While a POP3 user is logged in, a special file LOCK.!!! is created
in that user's mail directory. It is deleted when that user logs
out (or is timed out), and its only purpose is to prevent simultaneous access to
the same POP3 directory from two or more clients.

:p.:hp2.The multidomain case:ehp2.

:p.When you enable multidomain mode, the directory structure is
a little different. Each domain appears as a subdirectory of the
mail root directory. That subdirectory contains an INI file
containing information about that domain. The user directories are
also inside this directory. That is, the user directories are one
level deeper in the directory tree than in the single-domain
case. In all other respects, the descriptions above remain true
for the multidomain case. A separate 'postmaster' account (or a
suitable alias) is needed separately for each domain. There is
only one 'forward' directory, because mail to be forwarded belongs
to the system as a whole, rather than being identified with a
specific domain.

.***********************************
.*   BYTE-STUFFING
.***********************************

:h1 id=stuffing.Byte-stuffing
:hp2.Byte-stuffing:ehp2.

:p.Both the SMTP and POP3 protocols use a line containing the single
character '.' as an end-of-message marker. This raises the question
of what to do about an e-mail that contains such a line as part of
its body. We don't want it to be interpreted as a premature end of
message.

:p.To solve this problem, both mail protocols specify that an extra
'.' should be inserted at the beginning of any line that begins with
a '.' character. The extra character exists only while the mail is
in transit. It is inserted by the sender, and is removed by the
recipient. Normally you don't even need to know about it, because
the insertion and deletion is done by the program that you use to
read and send your e-mail.

:p.You do need to know about it if you are writing software that
will directly manipulate files in Weasel's mail directories. While
a message file is in a subdirectory of the Weasel mail root, it will
contain these extra '.' characters. If you take a file from there,
you'll probably need to remove those characters. If you insert a
file into the Weasel mail structure, you have to supply the extra
'.' in front of any line that starts with a '.'.

:p.The final end-marker '.' is :hp3.not:ehp3. stored in these files.
It is added by Weasel when sending the file on a tcp/ip connection.

.***********************************
.*   MAIL FILES IN THE 'FORWARD' DIRECTORY
.***********************************

:h1 id=forward.Mail files in the 'forward' directory
:hp2.Mail files in the 'forward' directory:ehp2.

:p.A file in the 'forward' directory differs from a mail file in a user
directory in two ways.

:ul compact.
:li.The file names end with ".FWD" instead of ".MSG".
:li.Each file starts with a special prefix, as described below.
:eul.

:p.In all other ways, these files are the same as the files in the
user directories.

:p.:hp2.The prefix string:ehp2.

:p.Mail waiting to be sent out is stored with a special prefix ahead
of the normal header lines. The format is as follows.

:xmp.
    4 bytes   format version, value 'V000'
    4 bytes   send time
    1 byte    retry number
    1 byte    flag byte, see below
    variable  sender, a character string
    variable  recipient list, bounded by () and comma-separated
:exmp.

:p.The message content starts immediately after this.

:p.The 'send time' is the time, measured in seconds since an arbitrary origin
set by the system software, when this item is due to be sent out. If the
send time is zero then this item will be queued to be sent out immediately,
with no delay.

:p.The 'retry number' is set to zero the first time this mail is queued
to be sent out, and is then incremented each time the system attempts to
send it out and does not succeed.

:p.The flags byte is a set of Boolean flags.
:dl.
:dt.  bits 7-2
:dd.unused in this version, should be 0
:dt.  bit 1
:dd.recirculate option
:dt.  bit 0
:dd.notify-on-failure flag
:edl.

:p.The 'notify-on-failure flag' is normally set to 1, to indicate that
an e-mail should be sent back to the sender if this item cannot be
delivered. If the flag is 0, no failure message is sent back to the
sender.

:p.The 'recirculate' option flag is normally 0. If it is set to 1,
Weasel will mail this item back to itself for further processing.
The main use for this option is to simplify some internal handling
where mail for multiple recipients has to be broken up into groups
that require different handling.

:p.You may, however, set the recirculate flag for mail deposited
directly into the forward directory in order to force that mail
to be filtered. Normally mail placed in the forward directory does
not undergo filtering, because 'forward' mail is presumed to have
already been through any necessary checks. If you set the
recirculate flag, such mail is forced to go through all processing
steps including the filtering. The recirculate flag will, of course,
be cleared by the time such a mail item is received for
reprocessing.

.***********************************
.*   SCANNING OF THE MAIL DIRECTORIES
.***********************************

:h1.Scanning of the mail directories
:hp2.Scanning of the mail directories:ehp2.

:p.When a POP user logs in to Weasel, that user's directory is
scanned to get a list of the *.MSG files. The list is stored
internally; if more mail arrives during the POP session, it will
be stored but will not be seen by the POP user. This restriction
is imposed because the POP3 protocol requires a stable message
base for the duration of a POP session. The new mail will, of
course, be seen the next time the user logs in.

:p.The 'forward' directory is a special case. Weasel re-checks
this directory every several minutes (the exact time might vary
from one version of Weasel to another). You can also force
Weasel to do an immediate check by using an event semaphore.
This is explained on the following page.

:p.If you wish to feed mail directly into the user directories,
independently of the Weasel mechanism, you can do so, and if
the file names end with '.MSG' then they will be detected the
next time the POP user logs in. Similarly, files in the
'forward' directory will be seen if their names end with '.FWD'.
It is, however, important that
these be complete files rather than half-written files. A good
way to ensure this is to open a new file with a name that does
:hp3.not:ehp3. end with '.MSG' or '.FWD', write to that file, and then
rename the file when the operation is finished. Another good way is
to create the new file as a hidden file, and unhide it after you
have finished creating it.

:p.(If you look at the 'forward' directory while Weasel is running, you
will find that most files in that directory are hidden. The Weasel
output module hides files while they are queued internally, to
ensure that duplicate copies won't be picked up on the
periodic scan of that directory.)


.***********************************
.*   EVENT SEMAPHORES
.***********************************

:h1.Event semaphores
:hp2.Event semaphores:ehp2.

:p.There are four event semaphores used by Weasel:

:dl break=all.
:dt.\SEM32\WEASEL\RECEIVED
:dd.Weasel posts this semaphore each time an item of mail is received.
:dt.\SEM32\WEASEL\FORWARDMAIL
:dd.Other programs can post this semaphore to force Weasel to do a
fresh check of the "forward" directory. (At the same time it does
a fresh check on whether it should be on-line.)
:dt.\SEM32\WEASEL\UPDATED
:dd.Other programs can post this semaphore to make Weasel re-read
its INI data from WEASEL.INI or WEASEL.TNI. This is how Setup.exe tells Weasel.exe
that there has been a change.
:dt.\SEM32\WEASEL\SHUTDOWN
:dd.Other programs can post this semaphore to force Weasel to
close itself down, exactly as if Ctrl/C had been typed at
the keyboard. If you post twice, this is the same as typing Ctrl/C
twice, and it causes Weasel to shut down faster because it aborts
any user sessions that are in progress, instead of waiting for
the users to log out.
:dt.\SEM32\WEASEL\FINISHED
:dd.Weasel posts this semaphore as its last operation before exiting.
:edl.

:p.:hp2.Note.:ehp2. These semaphores are :hp3.not:ehp3.  files. It is a
convention of the OS/2 operating system that the syntax for naming
event semaphores is similar to the syntax for file names, but apart from
this coincidence semaphores have nothing to do with files.

:p.The following Rexx code is an example of how you can post a semaphore.

:xmp.
call rxfuncadd 'rxuinit','rxu','rxuinit'
call rxuinit
SemName = "\SEM32\WEASEL\UPDATED"
if RxOpenEventSem(hev, SemName) \= 0 then
    rc = RxCreateEventSem( hev ,'Shared', SemName, 'Reset')
call RxPostEventSem hev
call RxResetEventSem hev
call RxCloseEventSem hev
call RxuTerm
:exmp.

:euserdoc.

